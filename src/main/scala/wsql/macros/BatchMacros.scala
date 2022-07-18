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

    def buildMethodType(paramss: List[ParamClause], tpt: TypeTree): MethodType =
      paramss match
        case head :: Nil =>
          MethodType(head.params.map(_.name))(
            (mt) => head.params.map(_.asInstanceOf[ValDef].tpt.tpe),
            (mt) => tpt.tpe)
        case Nil =>
          MethodType(Nil)( mt=>Nil, mt=> tpt.tpe)
        case others =>
          val init = others.init
          val last = others.last
          val tree = TypeTree.of[Any](using buildMethodType(List(last), tpt).asType.asInstanceOf[Type[Any]])
          buildMethodType(init, tree)


    // transform defdef method to return List[JdbcValue[?]] than SqlWithArgs
    val transform = new TreeMap :

      val symbolMapping = collection.mutable.Map[Symbol, Symbol]()

      override def transformStatement(tree: Statement)(owner: Symbol): Statement =
        tree match
          case tree@DefDef(name, paramss, tpt, rhs0) if tree.symbol.owner == Symbol.spliceOwner => // top level defdef
            val arg0 = paramss(0).params(0)  //

            val funcTpe = MethodType(List(arg0.symbol.name))(
              (mt) => List(TypeTree.of[T].tpe),
              (mt) => listOfJdbcValueTpt.tpe
            )
            val newDefDefSym = Symbol.newMethod(Symbol.spliceOwner, "$anonfun", funcTpe, Flags.EmptyFlags, Symbol.noSymbol)
            symbolMapping(tree.symbol) = newDefDefSym

            val rhsFn: List[List[Tree]] => Option[Term] = paramss =>
              symbolMapping( arg0.symbol ) = paramss(0)(0).symbol
              rhs0 match
                case Some(Block(stats, strContext: Apply)) =>
                  val newStats = transformStats(stats)(newDefDefSym)
                  val extract = extractJdbcValuesFromStringContext(strContext)
                  val extract2 = transformTerm(extract)(newDefDefSym)
                  Some(Block(newStats, extract2))
                case _ => None

            DefDef(newDefDefSym, rhsFn)

          // support defdef inside the lambda
          case tree@DefDef(name, paramss0, tpt, rhs0) if tree.symbol.owner != Symbol.spliceOwner =>
            val defSym = tree.symbol
            val owner = symbolMapping(tree.symbol.owner)
            val funcType = buildMethodType(paramss0, tpt )

            val newDefDefSym = Symbol.newMethod(owner, defSym.name, funcType, defSym.flags, Symbol.noSymbol)
            symbolMapping(defSym) = newDefDefSym

            val rhsFn: List[List[Tree]] => Option[Term] = paramss =>
              for( i <- 0 until paramss.size; j <- 0 until paramss(i).size)
                val param = paramss(i)(j)
                val param0 = paramss0(i).params(j)
                symbolMapping( param0.symbol ) = param.symbol

              rhs0.map(term => transformTerm(term)(owner))
            DefDef(newDefDefSym, rhsFn)

          case tree: ClassDef =>
            ???   // TODO add support for clone classdef

          // support valdef definition
          case tree@ValDef(name, tpt, rhs) =>
            val owner0 = tree.symbol.owner
            val owner = symbolMapping(owner0)
            val tpt1 = transformTypeTree(tree.tpt)(owner)
            val rhs1 = tree.rhs.map(x => transformTerm(x)(owner))

            val symbol = Symbol.newVal(owner, name, tpt1.tpe, Flags.Final, Symbol.noSymbol)
            val valdef = ValDef(symbol, rhs1)
            symbolMapping(tree.symbol) = symbol
            valdef

          case _ => super.transformStatement(tree)(owner)

      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case id@Ident(name) if symbolMapping contains id.symbol =>
            Ref( symbolMapping(id.symbol) )
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
