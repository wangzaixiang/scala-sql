package wangzx.scala_commons.sql

import scala.quoted.*
import java.sql.Connection

object Macros {

  def createBatchImpl[T: Type](proc: Expr[T=>SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =
    '{ null }

  def createMysqlBatchImpl[T: Type](proc: Expr[T=>SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =
    '{ null }

  def buildImpl1[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[Seq[(String, Any)]])(using Quotes): Expr[T] =
    '{ null.asInstanceOf[T] }

  def buildImpl2[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[T=>T])(using Quotes): Expr[T] =
    '{ null.asInstanceOf[T] }

}
