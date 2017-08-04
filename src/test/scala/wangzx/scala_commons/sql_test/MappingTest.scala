package wangzx.scala_commons.sql_test

import java.sql.ResultSet

import wangzx.scala_commons.sql.BeanMapping.ResultSetPrimitiveOperations
import wangzx.scala_commons.sql.{BeanMapping, IntEnum, JdbcValueMapper, JdbcValueMapperFactory, Table}

import scala.reflect.ClassTag

/**
  * Created by wangzx on 2016/11/30.
  */
object MappingTest {


  @Table
  class MyBean {

    var name: String = _
    var amount: BigDecimal = _
    var status: OrderStatus =  _ // status -> JdbcInt -> OrderStatus

    var notes: Option[String] = None  // TODO Option[...]

  }

  /**
    * mapping Int to Enum[T]
    *
    * 1. T has static apply(x: D): T method where D has JdbcValue[D] => T.apply( JdbcValueFactory[D].get(rs, fieldName) )
    * 2. T has public default constructor T(x: D) => T( JdbcValueAccessor[D].get(rs, field )
    * 3. T has implicit JdbcValueConverter[_, T] => using converter.fromResultSet(rs, field)
    */

  // How to construct BeanMapping[E]
  // for eacho var: T
  //   1. T is a basic JDBC Value Type (Int/Integer)
  //   2.1 has a implicitly JdbcValueFactory[T]
  //   2.2 -- has a related JdbcValueFactory[T] in context(ThreadLocal)
  //   3. via reflect: T has static method: fromResultSet(rs: ResultSet, index: String): T
  //   4. via reflect: T has default construtor and has instance method: fromResultSet(rs: ResultSet, index: String): T
  //   5. Container Type: scala.Option/java.util.Optional
  // all this can be done via scala-macro and generate code at compile-time
  // or 1, 3, 4 at runtime

 // This class Should be generate with scala-macro, this will using implicit JdbcValueFactory[X]
  // for Java Type, declare a static ""
  // which will support enum
  // and We also provide a method to generate BeanMapping via reflect at runtime
  object MyBean {

    implicit val some: BeanMapping2[MyBean] = new BeanMapping2[MyBean] {

      override def fromResultSet(rs: ResultSet) = {
        val myBean = new MyBean
        myBean.name = rs.getString("name") // myBean.name =

        // myBean.amount = // rs.getBigDecimal("amount") // implicit convert j.m.BigDecimal to scala.BigDecimal
        // myBean.amount = (t: Converter[JBig <: , Big]).from(

        myBean.amount = implicitly[JdbcValueConverter[_, BigDecimal]].fromResultSet(rs, "amount")

        myBean.status = OrderStatus.apply(rs.getInt("status"))

        myBean.notes =  new ScalaOptionFactory[String].fromResultSet(rs, "notes")

       myBean
      }

    }

  }

  // TODO support Option[T]
  private def rsCellToJavaValue(rs: ResultSet, idx: Int,
                                fieldMapping: BeanMapping[_]#FieldMapping[_],
                                jdbcValueMapperFactory: JdbcValueMapperFactory): Any = {
    import BeanMapping._

    val isNull = rs.getObject(idx) == null
    val rsPrimitiveOper = new ResultSetPrimitiveOperations(rs, idx)

    /**
      * fieldType match {
      *   case java.lang.Integer.TYPE => JdbcInt.get(rs, idx)
      *   case classOf[java.lang.Integer] => null or JdbcInt.get(rs.idx)
      *
      *   case classOf[scala.Option] => None or Some(JdbcXXX.get(rs.idx))
      *
      *   case ReflectConvertable(..)
      * }
      */

    if(isNull) fieldMapping.fieldType match {
      case java.lang.Boolean.TYPE => false
      case java.lang.Byte.TYPE => 0.toByte
      case java.lang.Short.TYPE  => 0.toShort
      case java.lang.Integer.TYPE => 0.toInt
      case java.lang.Long.TYPE => 0.toLong
      case java.lang.Float.TYPE => 0.toFloat
      case java.lang.Double.TYPE => 0.toDouble
      case ClassOfScalaOption => None
      case _ => null
    } else fieldMapping.fieldType match {
      case x if rsPrimitiveOper.isDefinedAt(x) =>
        rsPrimitiveOper.apply(x)

      case ClassOfScalaOption =>
        val prim = rsPrimitiveOper.apply(fieldMapping.optionalInnerType)
        Some(prim)

      case x if ClassOfJdbcValueMapper.isAssignableFrom(x) => //
        val rsValue = rs.getObject(idx)
        val obj = x.newInstance().asInstanceOf[JdbcValueMapper[AnyRef]]
        obj.getBeanValue(rs.getObject(idx), x.asInstanceOf[Class[AnyRef]])

      case x if jdbcValueMapperFactory.getJdbcValueMapper(x) != null =>
        val mapper = jdbcValueMapperFactory.getJdbcValueMapper(x).asInstanceOf[JdbcValueMapper[AnyRef]]
        mapper.getBeanValue(rs.getObject(idx), x.asInstanceOf[Class[AnyRef]])
    }
  }


  def main(args: Array[String]): Unit = {
//    dummySql(1)
//    dummySql(1, true)
//
//    val amount = BigDecimal(10)
//    dummySql(amount, OrderStatus.STATUS_1)
//
//    val rs: ResultSet = null
//
//    val myBean = rs2bean[MyBean](rs)
//    println(myBean)
//
//    val mf: ClassTag[Option[String]] = implicitly[ClassTag[Option[String]]]
//    println(mf)
//
//
//    val optionInt: CETest[Int, Option] = new CETest[Int, Option]()
  }


}
