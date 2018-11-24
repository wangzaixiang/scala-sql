package wangzx.scala_commons.sql

import java.sql.Connection

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import wangzx.scala_commons.sql._

object BatchMacro {

  // def createBatch[T](conn: Connection)(proc: T=>SQLWithArgs): Batch[T] = macro createBatchImpl[T]


  // convert a "insert into table set field1 = ?, field2 = ?"  as "insert into table(field1, fields) values(?,?)"

  def createBatchImpl[T: c.WeakTypeTag](c: Context)(conn: c.Tree)(proc: c.Tree) : c.Tree = {
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

    val result = q"""BatchImpl.apply($conn, ${transformer.statement})($changed) """

    result

  }

  def createConnectionBatchImpl[T: c.WeakTypeTag](c: Context)(proc: c.Tree): c.Tree = {
    import c.universe._
    val x = c.prefix.tree  // RichConnection
    createBatchImpl(c)(q"$x.conn")(proc)
  }

}
