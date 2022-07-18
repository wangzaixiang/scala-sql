package wsql.macros

import java.sql.Connection
import scala.quoted.{Expr, Quotes, Type}
import wsql.{Batch, SQLWithArgs}

object BatchMacros {

  // List[ JdbcValue[?] | Null ]
  private def listOfJdbcValueTpt(using quotes: Quotes) = {
    import quotes.reflect.*
    val jdbcValueTpt = TypeTree.of[ wsql.JdbcValue[?]|Null ]
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

  private def buildLamdbaOfJdbcValues[T: Type](using quotes: Quotes)(lambdaOfSqlWithArgs: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*

    // transform defdef method to return List[JdbcValue[?]] than SqlWithArgs
    val transform = new TreeMap :

      var oldDefDefSym: Symbol | Null = null // old defdef symbol
      val defMapping = collection.mutable.Map[String, Symbol]()  // symbols inside oldDefDef should mapping to news inside newDefDef

      override def transformStatement(tree: Statement)(owner: Symbol): Statement =
        tree match
          case tree@DefDef(name, paramss, tpt, rhs0) =>
            val arg0 = paramss(0).params(0)

            this.oldDefDefSym = tree.symbol
            val funcTpe = MethodType(List("arg"))(
              (mt) => List(TypeTree.of[T].tpe),
              (mt) => listOfJdbcValueTpt.tpe
            )
            val newDefDefSym = Symbol.newMethod(Symbol.spliceOwner, "$anonfun", funcTpe, Flags.EmptyFlags, Symbol.noSymbol)
            val rhsFn: List[List[Tree]] => Option[Term] = paramss =>
              defMapping(arg0.name) = paramss(0)(0).symbol
              rhs0 match
                case Some(Block(stats, strContext: Apply)) =>
                  val newStats = transformStats(stats)(newDefDefSym)
                  val extract = extractJdbcValuesFromStringContext(strContext)
                  val extract2 = transformTerm(extract)(newDefDefSym)
                  Some(Block(newStats, extract2))
                case _ => None

            // this.defSymbol = anonymousMethodSymbol
            defMapping(tree.name) = newDefDefSym
            val defdef = DefDef(newDefDefSym, rhsFn)
            defdef

          case tree@ValDef(name, tpt, rhs) if tree.symbol.owner == oldDefDefSym =>
            val owner = defMapping(oldDefDefSym.nn.name)
            val tpt1 = transformTypeTree(tree.tpt)(owner)
            val rhs1 = tree.rhs.map(x => transformTerm(x)(owner))

            val symbol = Symbol.newVal(owner, name, tpt1.tpe, Flags.Final, Symbol.noSymbol)
            val valdef = ValDef(symbol, rhs1)
            defMapping(name) = symbol
            valdef

          case _ => super.transformStatement(tree)(owner)

      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case Ident(name) if tree.symbol.owner == oldDefDefSym =>
            defMapping.get(name) match
              case Some(symbol) => Ref(symbol)
              case _ => ???
          case Closure(meth, tpt) if meth.symbol == oldDefDefSym =>
            Closure(Ref( defMapping(oldDefDefSym.nn.name) ), None)
          case _ => super.transformTerm(tree)(owner)

      // extract args from StringContext(parts).apply(args)
      def extractJdbcValuesFromStringContext(apply: Apply): Apply =
        val jdbcValueTpt = TypeTree.of[ wsql.JdbcValue[?] | Null]
        apply match
          case Apply(sc, args) =>
            Apply(
              TypeApply(
                Select.unique(Ref(Symbol.requiredModule("scala.collection.immutable.List")), "apply"),
                List( jdbcValueTpt )
              ), args
            )
    end transform

    val result = transform.transformTerm(lambdaOfSqlWithArgs)(Symbol.spliceOwner)
    result

  def createBatchImpl[T: Type](proc: Expr[T => SQLWithArgs], conn: Expr[Connection])(using quotes: Quotes): Expr[Batch[T]] =
    import quotes.reflect.*

    val lambda = proc.asTerm match
      case Inlined(_, _, Block(Nil, lambdaOfSqlWithArgs)) =>
        val result = buildLamdbaOfJdbcValues(lambdaOfSqlWithArgs)
        result

      case _ =>
        ???

    val expr = buildBatchExpr(conn, lambda)
    expr

  def createMysqlBatchImpl[T: Type](proc: Expr[T => SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =
    '{???}


}
