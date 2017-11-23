package wangzx.scala_commons.sql_test

import java.sql.ResultSet

/**
  * Created by wangzx on 2016/12/1.
  */
object ConvertTest {

  trait JdbcValueAccessor[T]

  trait JdbcValue[T] extends JdbcValueAccessor[T] {
    val value: T
  }

  class JdbcValueOthers[T: JdbcValueAccessor](val value: T) extends JdbcValue[T]

  implicit class JdbcInt(val value: Int) extends JdbcValue[Int]
  implicit object JdbcIntAccessor extends JdbcValueAccessor[Int]

  implicit def any2value[T: JdbcValueAccessor](t: T) = new JdbcValueOthers(t)

  // val i: JdbcInt[_] = 10
  // i = new JdbcInt(10) -- it is choiced
  // i = any2value[JdbcInt](10)

  implicit class JdbcString(val value: String) extends JdbcValue[String]
  implicit object JdbcStringAccessor extends JdbcValueAccessor[String]

  implicit object ScalaBigDecimalAccessor extends JdbcValueAccessor[BigDecimal]
  val x = BigDecimal(10)

  def dummySql(args: JdbcValue[_]*): Unit = {

  }

  implicit class SqlWrapper(sc: StringContext) {
    def sql(args: JdbcValue[_]*): Unit = {

    }
  }

  class Foo
  object Foo {
    // implicit value Feature[Foo] may be define in Object Foo
    implicit object FooAccessor extends JdbcValueAccessor[Foo]
  }

  // this will override Foo.implicits
  // implicit object FooAccessor2 extends JdbcValueAccessor[Foo]


  // val i = rs.get[Int](index)

  implicit class ResultSetExt(rs: ResultSet) {
    def get[T : JdbcValueAccessor](index: Int): T = {
      null.asInstanceOf[T]
    }
    def getOptional[T: JdbcValueAccessor](index: Int): Option[T] = None
  }

  implicit class OptionValue[T <% JdbcValue[T] ](opt: Option[T]) extends JdbcValue[T] {
    val value = opt.getOrElse(null.asInstanceOf[T])
  }




  val rs: ResultSet = null

  def doSth(): Unit ={
    val i: Int = rs.get[Int](1)
    val bd: BigDecimal = rs.get[BigDecimal](2)

    val notes: Option[String] = rs.getOptional[String](3)
    val foo = new Foo

    // val notes2: OptionValue[String] = notes

    dummySql(1, x, foo, notes)
    // ....

    sql"select * from user where name in ($i, $bd, $foo, $notes)"

  }

}
