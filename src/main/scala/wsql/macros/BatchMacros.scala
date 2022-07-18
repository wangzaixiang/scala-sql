package wsql.macros

import wsql.{Batch, SQLWithArgs}

import java.sql.Connection
import scala.quoted.{Expr, Quotes, Type}

object BatchMacros {

  // List[ JdbcValue[?] ]
  private def listOfJdbcValueTpt(using quotes: Quotes) = {
    import quotes.reflect.*
    val jdbcValueTpt = Applied(TypeIdent(Symbol.requiredClass("wsql.JdbcValue")),
      List(TypeBoundsTree(TypeTree.of[Nothing], TypeTree.of[Any]))
    )
    Applied(TypeIdent(Symbol.requiredClass("scala.collection.immutable.List")), List(jdbcValueTpt))
  }

  private def buildBatchExpr[T: Type](using quotes: Quotes)(conn: Expr[Connection], arg2JdbcValuesLambda: quotes.reflect.Term): Expr[Batch[T]] =
    import quotes.reflect.*

    val funcTpt = Applied(TypeIdent(Symbol.requiredClass("scala.Function1")),
      List(TypeTree.of[T], listOfJdbcValueTpt)
    )
    val symbolFunc = Symbol.newVal(Symbol.spliceOwner, "func", funcTpt.tpe, Flags.EmptyFlags, Symbol.noSymbol)

    val valdef = ValDef(symbolFunc, Some(arg2JdbcValuesLambda))
    val apply = Apply(
      TypeApply(Select.unique(Ref(Symbol.requiredModule("wsql.BatchImpl")), "apply"),
        List(TypeTree.of[T])
      ),
      List(conn.asTerm, Literal(StringConstant("select stmt")),
        Ref(symbolFunc)
      )
    )
    val expr = Block(List(valdef), apply)

    expr.asExpr.asInstanceOf[Expr[Batch[T]]]

  private def buildLamdba[T: Type](using quotes: Quotes)(lambdaBlock: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*

    // transform defdef method to return List[JdbcValue[?]] than SqlWithArgs
    val transform = new TreeMap :

      var defSymbol0: Symbol | Null = null // old defdef symbol
      var defSymbol: Symbol | Null = null // new defdef symbpl
      var defdefParamss: List[List[Tree]] | Null = null
      var childSymbols = collection.mutable.Map[String, Symbol]()

      override def transformStatement(tree: Statement)(owner: Symbol): Statement =
        tree match
          case tree@DefDef(name, paramss, tpt, rhs0) =>
            this.defSymbol0 = tree.symbol
            val funcTpe = MethodType(List("arg"))(
              (mt) => List(TypeTree.of[T].tpe),
              (mt) => listOfJdbcValueTpt.tpe
            )
            // val owner = Symbol.spliceOwner
            val anonymousMethodSymbol = Symbol.newMethod(Symbol.spliceOwner, "$AnonFun", funcTpe, Flags.EmptyFlags, Symbol.noSymbol)
            val rhsFn: List[List[Tree]] => Option[Term] = paramss =>
              this.defdefParamss = paramss
              rhs0 match
                case Some(Block(stats, strContext: Apply)) =>
                  val newStats = transformStats(stats)(anonymousMethodSymbol)
                  val extract = extractJdbcValuesFromStringContext(strContext)
                  val extract2 = transformTerm(extract)(anonymousMethodSymbol)
                  Some(Block(newStats, extract2))
                case _ => None

            this.defSymbol = anonymousMethodSymbol
            val defdef = DefDef(anonymousMethodSymbol, rhsFn)
            defdef

          case tree@ValDef(name, tpt, rhs) =>
            val tpt1 = transformTypeTree(tree.tpt)(defSymbol.nn)
            val rhs1 = tree.rhs.map(x => transformTerm(x)(defSymbol.nn))

            val symbol = Symbol.newVal(defSymbol.nn, name, tpt1.tpe, Flags.Final, Symbol.noSymbol)
            val valdef = ValDef(symbol, rhs1)
            childSymbols(name) = symbol
            valdef

          case _ => super.transformStatement(tree)(owner)

      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case a@Ident("u") =>
            val a = Ref(this.defdefParamss.nn.apply(0).apply(0).symbol) // TODO replace u
            a
          case Ident(name) if tree.symbol.owner == defSymbol0 =>
            childSymbols.get(name) match
              case Some(symbol) => Ref(symbol)
              case _ => ???
          case Closure(meth, tpt) =>
            Closure.copy(tree)(Ref(defSymbol.nn), None)
          case _ => super.transformTerm(tree)(owner)

      // extract args from StringContext(parts).apply(args)
      def extractJdbcValuesFromStringContext(apply: Apply): Apply =
        apply match
          case Apply(sc, args) =>
            Apply(
              TypeApply(
                Select.unique(Ref(Symbol.requiredModule("scala.collection.immutable.List")), "apply"),
                List(Applied(TypeIdent(Symbol.requiredClass("wsql.JdbcValue")), List(TypeBoundsTree(TypeTree.of[Nothing], TypeTree.of[Any]))))
              ), args
            )
    end transform

    val result = transform.transformTerm(lambdaBlock)(Symbol.spliceOwner)
    result


  def createBatchImpl[T: Type](proc: Expr[T => SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =

    val lambda = proc.asTerm match
      case Inlined(_, _, Block(Nil, lambdaBlock)) =>
        val result = buildLamdba(lambdaBlock)
        result

      case _ =>
        ???

    val expr = buildBatchExpr(conn, lambda)
    expr

  def createMysqlBatchImpl[T: Type](proc: Expr[T => SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =
    '{???}


}
