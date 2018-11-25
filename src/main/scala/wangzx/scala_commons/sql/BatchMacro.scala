package wangzx.scala_commons.sql

import java.sql.Connection
import java.util

import net.sf.jsqlparser.expression.Expression
import net.sf.jsqlparser.expression.operators.relational.ExpressionList
import net.sf.jsqlparser.parser.{CCJSqlParser, CCJSqlParserUtil}
import net.sf.jsqlparser.schema.Column
import net.sf.jsqlparser.statement.Statement
import net.sf.jsqlparser.statement.insert.Insert

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import wangzx.scala_commons.sql._

import scala.collection.JavaConverters._


object BatchMacro {

  // def createBatch[T](conn: Connection)(proc: T=>SQLWithArgs): Batch[T] = macro createBatchImpl[T]

  def rewriteMySQLInsert(stmt: String): String = {
    val insert: Insert = CCJSqlParserUtil.parse(stmt).asInstanceOf[Insert]

    assert(insert.getSelect == null, "not support insert .. select ... statement")

    if(insert.isUseSet){ // rewrite as values(...)
      // columns
      // itemsList

      val columns = insert.getSetColumns
      val itemsList: util.List[Expression] = insert.getSetExpressionList

      insert.setSetColumns(null)
      insert.setSetExpressionList(null)
      insert.setUseSet(false)

      insert.setUseValues(true)
      insert.setColumns(columns)
      insert.setItemsList(new ExpressionList(itemsList))
    }

    insert.toString
  }

  // convert a "insert into table set field1 = ?, field2 = ?"  as "insert into table(field1, fields) values(?,?)"

  private def createBatchImpl0[T: c.WeakTypeTag](c: Context)(conn: c.Tree, proc: c.Tree, convertMysql: Boolean) : c.Tree = {
    import c.universe._

    val t: c.WeakTypeTag[T] = implicitly[c.WeakTypeTag[T]]

    object transformer extends Transformer {

      var statement: String = null

      override def transform(tree: c.universe.Tree): c.universe.Tree = tree match {

        // tree is type checked
        case i @ q"""wangzx.scala_commons.sql.`package`.SQLStringContext(scala.StringContext.apply(..${strings:List[c.Tree]})).sql(..$args)""" =>

          val statement = strings.map {
            case Literal(Constant(str: String)) =>
              str
            case _ =>
              c.error(c.enclosingPosition, "not a string literal")
          }.mkString("?")

//          println(s"statement = $statement")

          if(this.statement == null) {
            this.statement = statement
          }
          else {
            c.error(c.enclosingPosition, """only 1 sql interpolation supported""")
          }

          q"""scala.collection.immutable.List.apply[JdbcValue[_]]( ..$args )"""

        case _ => super.transform(tree)
      }
    }

    val expectType = implicitly[c.WeakTypeTag[ T=>List[JdbcValue[_]] ] ]

    val changed = c.typecheck( c.untypecheck( transformer.transform(proc) ) )

    if( changed.tpe =:= expectType.tpe ) {
    }
    else {
      println(s"changed.tpe = ${changed.tpe}")
      println(s"expected type = ${expectType.tpe}")
      c.error(c.enclosingPosition, "function is too complex for process, see documents")
    }

    if(transformer.statement == null){
      c.error(c.enclosingPosition, "no sql interpolation defined in block")
    }

    val statement =
      if(convertMysql) rewriteMySQLInsert(transformer.statement)
      else transformer.statement

    val result = q"""BatchImpl.apply($conn, ${statement})($changed) """

    result

  }

  def createBatchImpl[T: c.WeakTypeTag](c: Context)(proc: c.Tree): c.Tree = {
    import c.universe._
    val x = c.prefix.tree  // RichConnection
    createBatchImpl0(c)(q"$x.conn", proc, false)
  }

  def createMySqlBatchImpl[T: c.WeakTypeTag](c: Context)(proc: c.Tree): c.Tree = {
    import c.universe._
    val x = c.prefix.tree
    createBatchImpl0(c)(q"$x.conn", proc, true)
  }

}
