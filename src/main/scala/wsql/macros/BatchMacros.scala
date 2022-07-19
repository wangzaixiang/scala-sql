package wsql.macros

import net.sf.jsqlparser.expression.operators.relational.ExpressionList
import net.sf.jsqlparser.parser.CCJSqlParserUtil
import net.sf.jsqlparser.statement.insert.Insert

import java.sql.Connection
import scala.quoted.{Expr, Quotes, Type}
import wsql.{Batch, JdbcValue, SQLWithArgs}

object BatchMacros {

  type ListOfJdbcValues = List[wsql.JdbcValue[?]|Null]

  private def buildBatchExpr[T: Type](using quotes: Quotes)(conn: Expr[Connection], sql: String, arg2JdbcValuesLambda: quotes.reflect.Term): Expr[Batch[T]] =
    import quotes.reflect.*

    val funcTpt = TypeTree.of[ (T) => ListOfJdbcValues ]
    val symbolFunc = Symbol.newVal(Symbol.spliceOwner, "func", funcTpt.tpe, Flags.EmptyFlags, Symbol.noSymbol)

    val valdef = ValDef(symbolFunc, Some(arg2JdbcValuesLambda))
    val apply = Apply(
      TypeApply(Select.unique(Ref(Symbol.requiredModule("wsql.BatchImpl")), "apply"),
        List(TypeTree.of[T])
      ),
      List(conn.asTerm, Literal(StringConstant(sql)), Ref(symbolFunc)
      )
    )
    val expr = Block(List(valdef), apply)
    expr.asExpr.asInstanceOf[Expr[Batch[T]]]

  private def extractSql(using quotes:Quotes)(tree: quotes.reflect.Term): String =
    import quotes.reflect.*
    tree match
      case tree@Lambda(params, Block(stats, sc:Apply)) =>
        sc match
          case Apply(Apply(sql, List(Apply(scApply, List(Typed(Repeated(sqls,_),_)) ))), _) =>
            sqls.map{ case Literal(StringConstant(s)) => s }.mkString("?")
          case Apply(Select(Ident("SQLWithArgs"), "apply"),
            List(Literal(StringConstant(s)), _) ) => s
          case _ => throw new RuntimeException(s"""expect sql"statement" or SQLWithArgs(literal, args)""")

  private def extractJdbcValues(using quotes:Quotes)(apply: quotes.reflect.Apply): quotes.reflect.Apply =
    import quotes.reflect.*

    val args = apply match
      case Apply( Apply( id@Ident("sql"), List(stringContext)), args) if(id.symbol == Symbol.requiredMethod("wsql.sql")) =>
        args
      case Apply(Select(sqlWithArgs, "apply"), List(sql_stat, Apply(seqApply, args) )) if sqlWithArgs.tpe =:= TypeRepr.of[SQLWithArgs.type ] =>
        args
      case _ =>
        println(apply.show(using Printer.TreeStructure))
        throw new RuntimeException("""support sql"..." or SqlWithArgs(sql, args) only""")

    Apply(
      TypeApply(
        Select.unique(Ref(Symbol.requiredModule("scala.collection.immutable.List")), "apply"),
        List(TypeTree.of[wsql.JdbcValue[?] | Null])
      ), args
    )

  private def buildLambdaOfJdbcValues[T: Type](using quotes: Quotes)(lambdaOfSqlWithArgs: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*

    lambdaOfSqlWithArgs match
      case tree@Lambda(params, body) => // transform to new lambda with new mt and body
        val paramNames = params.map(_.name)
        val paramTypes = params.map(_.tpt.tpe)
        val mt = MethodType(paramNames)( (mt) => paramTypes, (mt) => TypeRepr.of[ListOfJdbcValues] )
        Lambda( Symbol.spliceOwner, mt, (meth, args)=> changeBody(params, args, body).changeOwner(meth) )

      case _ => throw new RuntimeException(("expect lambda"))

  private def changeBody(using quotes:Quotes)(oldArgs: List[quotes.reflect.Tree], newArgs: List[quotes.reflect.Tree],
                                              body: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    val associations: Map[Symbol, Term] = oldArgs.zip(newArgs).foldLeft(Map.empty){
      case (m, (oldParam, newParam: Term)) => m.updated(oldParam.symbol, newParam)
      case (m, (oldParam, newParam: Tree)) => throw new RuntimeException("Term expected")
    }

    val changes = new TreeMap :
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case id@Ident(name) => associations.getOrElse(id.symbol, super.transformTerm(tree)(owner))
          case _ => super.transformTerm(tree)(owner)


    body match
      case Block(stats, stringContextApply: Apply) =>
        val newStats = changes.transformStats(stats)(Symbol.spliceOwner)
        val extract = extractJdbcValues(stringContextApply)
        val extract2 = changes.transformTerm(extract)(Symbol.spliceOwner)
        Block(newStats, extract2)


  def createBatchImpl[T: Type](proc: Expr[T => SQLWithArgs],
                               conn: Expr[Connection],
                               convertMySqlInsert: Boolean
                              )(using quotes: Quotes): Expr[Batch[T]] =
    import quotes.reflect.*

    proc.asTerm match
      case Inlined(_, _, Block(Nil, lambdaOfSqlWithArgs)) =>
        val lambda = buildLambdaOfJdbcValues(lambdaOfSqlWithArgs)
        val sql = extractSql(lambdaOfSqlWithArgs)
        val rewrited = if convertMySqlInsert then rewriteMySQLInsert(sql) else sql
        buildBatchExpr(conn, rewrited, lambda)

      case _ =>
        ???

  private def rewriteMySQLInsert(stmt: String): String =
    val insert: Insert = CCJSqlParserUtil.parse(stmt).asInstanceOf[Insert]

    assert(insert.getSelect == null, "not support insert .. select ... statement")
    if(insert.isUseSet)
      val columns = insert.getSetColumns.nn
      val itemsList = insert.getSetExpressionList
      insert.setSetColumns(null)
      insert.setSetExpressionList(null)
      insert.setUseSet(false)

      insert.setUseValues(true)
      insert.setColumns(columns)
      insert.setItemsList(new ExpressionList(itemsList))
    insert.toString

}
