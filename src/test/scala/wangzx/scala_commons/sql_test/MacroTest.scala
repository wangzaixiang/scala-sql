package wangzx.scala_commons.sql_test

import java.sql.{ResultSet, ResultSetMetaData}
import wsql.*

import java.net.URL

object MacroTest {

  case class User(name: java.lang.String, age: Option[Int], classRoom: Int = 1)

  def getMetadata(data: Map[String, Any]): ResultSetMetaData = new ResultSetMetaData:
    override def getColumnCount: Int = data.size
    override def isAutoIncrement(column: Int): Boolean = false
    override def isCaseSensitive(column: Int): Boolean = false
    override def isSearchable(column: Int): Boolean = false
    override def isCurrency(column: Int): Boolean = false
    override def isNullable(column: Int): Int = 0
    override def isSigned(column: Int): Boolean = false
    override def getColumnDisplaySize(column: Int): Int = ???
    override def getColumnLabel(column: Int): String = data.keySet.toList.sorted.apply(column - 1)
    override def getColumnName(column: Int): String = data.keySet.toList.sorted.apply(column - 1)
    override def getSchemaName(column: Int): String = ???
    override def getPrecision(column: Int): Int = ???
    override def getScale(column: Int): Int = ???
    override def getTableName(column: Int): String = ???
    override def getCatalogName(column: Int): String = ???
    override def getColumnType(column: Int): Int = ???
    override def getColumnTypeName(column: Int): String = ???
    override def isReadOnly(column: Int): Boolean = false
    override def isWritable(column: Int): Boolean = false
    override def isDefinitelyWritable(column: Int): Boolean = false
    override def getColumnClassName(column: Int): String = ???
    override def unwrap[T](iface: Class[T]): T = ???
    override def isWrapperFor(iface: Class[_]): Boolean = ???

  def getResultSet(data: Map[String, Any]): ResultSet = java.lang.reflect.Proxy.newProxyInstance(getClass.getClassLoader, Array(classOf[ResultSet]),
      (proxy: Any, method: java.lang.reflect.Method, args: Array[AnyRef]) => {
        inline def fieldName(args: Array[AnyRef]): String = args(0).asInstanceOf[String]
        method.getName match {
          case "getString" => data(fieldName(args))
          case "getInt" => data(fieldName(args))
          case "getLong" => data(fieldName(args))
          case "getObject" => data(fieldName(args))
          case "getMetaData" => getMetadata(data)
          case _ => throw new Exception(s"method ${method} not support")
        }
      }).asInstanceOf[ResultSet]

  @main
  def test1(): Unit ={
    // val userMapper = ResultSetMapperMacro.resultSetMapper[User]
    def test[T: ResultSetMapper](data: Map[String, Any]): T =
      val mapper = summon[ResultSetMapper[T]]
      mapper.from(getResultSet(data))

    val data = Map("name" -> "wangzx", "age" -> 18, "classRoom" -> 2)
    assert( test[User](data) == User("wangzx", Some(18), 2) )

    val data2 = Map("name" -> "wangzx", "age" -> 18)
    assert( test[User](data) == User("wangzx", Some(18), 1) )

    val data3 = Map("name" -> "wangzx")
    assert( test[User](data) == User("wangzx", None, 1) )

    println("passed")
  }

}
