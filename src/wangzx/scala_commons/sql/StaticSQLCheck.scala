package wangzx.scala_commons.sql

import javax.sql.DataSource
import scala.reflect.macros.Context
import scala.language.experimental.macros

/**
 * provide a compile time SQL syntax checking using the underlying database
 * 
 * usage:
 * 1. create a "sqlcheck.properties" file contains these entries:
 *    default.driver, default.url, default.user, default.password
 * 2. create your sql"" implementation like StaticCheck.SQLStringContextWithCheck
 */
object StaticSQLCheck {
  
  implicit class SQLStringContextWithCheck(sc: StringContext) {
    def sql(args: Any*) = macro checkSQL
  }
  
  def checkSQL(c: Context)(args: c.Expr[Any]*): c.Expr[SQLWithArgs] = 
    checkSQLImpl(c)(args:_*)("default")
  
    
  def checkSQLImpl(c: Context)(args: c.Expr[Any]*)(db: String): c.Expr[SQLWithArgs] = {
    import c.universe._
    val stmt = c.prefix.tree match {
      case Apply(_, List(Apply(_,args@List(_*)))) =>
        args.map { case Literal(Constant(const: String)) => const
        case _ => c.abort(c.enclosingPosition, "invalid string literal")
        }.mkString("?")
      case _ => c.abort(c.enclosingPosition, "invalid string literal")
    }
    println(s"Check SQL:[$stmt]")
    // TODO using database connection to check the SQL statement
    val stmtExpr:c.Expr[String] = c.Expr(Literal(Constant(stmt)))
    
    val seq = Apply(Select(Select(Select(Ident("scala"), newTermName("collection")),
        newTermName("Seq")),newTermName("apply")), args.toList.map(_.tree))
    val seqExpr = c.Expr(seq)

    c.universe.reify {
      SQLWithArgs(stmtExpr.splice, seqExpr.splice)
    }
  }
  
}