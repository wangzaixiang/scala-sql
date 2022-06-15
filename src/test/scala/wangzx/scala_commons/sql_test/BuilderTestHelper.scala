package wangzx.scala_commons.sql_test

import scala.quoted.*
import BuilderTest.*

object BuilderTestHelper {

  inline def build1[T](source: AnyRef): T = ${ build1Impl[T]('source) }

  def build1Impl[T: Type](source: Expr[AnyRef])(using Quotes): Expr[T] =
    import quotes.reflect.*

    def select(source: Expr[AnyRef], name: String): Expr[T] =
      Select.unique(source.asTerm, name ).asExpr.asInstanceOf[Expr[T]]

    def dump[T](source: Expr[T]): Expr[T] =
      println(source.show)
      println(source.asTerm.show(using Printer.TreeStructure))
      source

    dump('{
    val f1 = ${ select(source, "province").asInstanceOf[Expr[String]] }
    val f2 = ${ select(source, "city").asInstanceOf[Expr[String]] }
      PersonAddress(f1, f2).asInstanceOf[T]
    })


}
