package wangzx.scala_commons.sql

import java.lang.reflect.Method
import java.lang.annotation.Annotation
import java.beans.Introspector
import java.sql.{Connection, ResultSet, ResultSetMetaData}
import javax.sql.DataSource
import wangzx.scala_commons.sql.BeanMapping._

import scala.ref.SoftReference
import scala.reflect.ClassTag

/**
 * provide a simple Object-Entity-Mapping without relationship
 */
object BeanMapping {

  val ClassOfByte = classOf[java.lang.Byte]
  val ClassOfChar = classOf[java.lang.Character]
  val ClassOfShort = classOf[java.lang.Short]
  val ClassOfInteger = classOf[java.lang.Integer]
  val ClassOfLong = classOf[java.lang.Long]
  val ClassOfFloat = classOf[java.lang.Float]
  val ClassOfDouble = classOf[java.lang.Double]
  val ClassOfBoolean = classOf[java.lang.Boolean]
  val ClassOfString = classOf[java.lang.String]
  val ClassOfSQLDate = classOf[java.sql.Date]
  val ClassOfUtilDate = classOf[java.util.Date]
  val ClassOfSQLTime = classOf[java.sql.Date]
  val ClassOfSQLTimestamp = classOf[java.sql.Timestamp]
  val ClassOfBigDecimal = classOf[java.math.BigDecimal]
  val ClassOfScalaBigDecimal = classOf[scala.math.BigDecimal]
  val ClassOfByteArray = classOf[Array[Byte]]
  val ClassOfJdbcValueMapper = classOf[JdbcValueMapper[_]]

  val G_BeanMappings = new SoftMap[Class[_], BeanMapping[_]]()

  /**
   * for a scala anonymous class, automate choose the parent
   */
  val annomous_regexp = """anon\$\d+""".r
  def real_class(clazz: Class[_]): Class[_] = clazz.getSimpleName match {
    case annomous_regexp() => real_class(clazz.getSuperclass)
    case _ => clazz
  }

  def isSupportedDataType(typ: Class[_]): Boolean = typ match {
    case java.lang.Boolean.TYPE | ClassOfBoolean => true
    case java.lang.Byte.TYPE | ClassOfByte => true
    case java.lang.Short.TYPE | ClassOfShort => true
    case java.lang.Integer.TYPE | ClassOfInteger => true
    case java.lang.Long.TYPE | ClassOfLong => true
    case java.lang.Float.TYPE | ClassOfFloat => true
    case java.lang.Double.TYPE | ClassOfDouble => true
    case ClassOfBigDecimal => true
    case ClassOfScalaBigDecimal => true
    case ClassOfSQLDate => true
    case ClassOfSQLTime => true
    case ClassOfSQLTimestamp | ClassOfUtilDate => true
    case ClassOfString => true
    case ClassOfByteArray => true
    case _ => ClassOfJdbcValueMapper.isAssignableFrom(typ)
  }

  /**
   * @param factory provide a custom
   */
  def getBeanMapping[T](clazz: Class[T])(factory: JdbcValueMapperFactory = NullJdbcValueMapperFactory): BeanMapping[T] = {
    synchronized {
      val cached: Option[BeanMapping[_]] = G_BeanMappings.get(clazz)
      cached match {
        case Some(result) =>
          result.asInstanceOf[BeanMapping[T]]

        case None =>
          val realClass = real_class(clazz)
          val mapping = new UnionBeanMapping(realClass)(factory)
          G_BeanMappings(clazz) = mapping
          return mapping.asInstanceOf[BeanMapping[T]]
      }
    }
  }

  // userName -> user_name
  def mappingCamelToUnderscore(camelName: String): String = {
    val builder = StringBuilder.newBuilder

    var pos = 0
    var isLastUpper = false

    assert(camelName.length >= 1)

    var first = camelName.charAt(0)
    builder.append(first)
    isLastUpper = first >= 'A' && first <= 'Z'

    pos += 1
    while(pos < camelName.length){
      var ch = camelName.charAt(pos)
      if(ch >= 'A' && ch <= 'Z') {
        val lower: Char = (ch + 'a' - 'A').toChar
        if(isLastUpper == false) builder.append('_').append(lower)
        else builder.append(lower)
        isLastUpper = true
      }
      else {
        builder.append(ch)
        isLastUpper = false
      }
      pos += 1
    }
    builder.toString
  }

  /**
   * mapping from ResultSet to JavaBean, field is JavaBean Property, and may annotated with @Column
   */
  def rs2bean[T: ClassTag](rsMeta: ResultSetMetaData, rs: ResultSet, jdbcValueMapperFactory: JdbcValueMapperFactory): T = {
    val bean: T = implicitly[ClassTag[T]].runtimeClass.newInstance().asInstanceOf[T]

    bean match {
      case rsConvertable: ResultSetConvertable =>
        rsConvertable.fromResultSet(rs)
      case _ =>
        val beanMapping = BeanMapping.getBeanMapping(bean.getClass)(jdbcValueMapperFactory).asInstanceOf[BeanMapping[T]]
        for (idx <- 1 to rsMeta.getColumnCount) {
          val label = rsMeta.getColumnLabel(idx).toLowerCase()
          beanMapping.getFieldByColumnName(label) match {
            case Some(fieldMapping) =>
              val value = rsCellToJavaValue(rs, idx, fieldMapping, jdbcValueMapperFactory)
              fieldMapping.asInstanceOf[beanMapping.FieldMapping[Any]].set(bean, value)
            case None =>
              // no matched field, so the property will be null or default
          }
        }
    }

    bean
  }

  private def rsCellToJavaValue(rs: ResultSet, idx: Int,
             fieldMapping: BeanMapping[_]#FieldMapping[_],
             jdbcValueMapperFactory: JdbcValueMapperFactory): Any = {

    val isNull = rs.getObject(idx) == null

    if(isNull) fieldMapping.fieldType match {
      case java.lang.Boolean.TYPE => false
      case java.lang.Byte.TYPE => 0.toByte
      case java.lang.Short.TYPE  => 0.toShort
      case java.lang.Integer.TYPE => 0.toInt
      case java.lang.Long.TYPE => 0.toLong
      case java.lang.Float.TYPE => 0.toFloat
      case java.lang.Double.TYPE => 0.toDouble
      case _ => null
    } else fieldMapping.fieldType match {
      case java.lang.Boolean.TYPE | ClassOfBoolean => rs.getBoolean(idx)
      case java.lang.Byte.TYPE | ClassOfByte => rs.getByte(idx)
      case java.lang.Short.TYPE | ClassOfShort => rs.getShort(idx)
      case java.lang.Integer.TYPE | ClassOfInteger => rs.getInt(idx)
      case java.lang.Long.TYPE | ClassOfLong => rs.getLong(idx)
      case java.lang.Float.TYPE | ClassOfFloat => rs.getFloat(idx)
      case java.lang.Double.TYPE | ClassOfDouble => rs.getDouble(idx)
      case ClassOfBigDecimal => rs.getBigDecimal(idx)
      case ClassOfScalaBigDecimal => scala.math.BigDecimal(rs.getBigDecimal(idx))
      case ClassOfSQLDate => rs.getDate(idx)
      case ClassOfSQLTime => rs.getTime(idx)
      case ClassOfSQLTimestamp | ClassOfUtilDate => rs.getTimestamp(idx)
      case ClassOfString => rs.getString(idx)
      case ClassOfByteArray => rs.getBytes(idx)

      case x if ClassOfJdbcValueMapper.isAssignableFrom(x) => //
        val rsValue = rs.getObject(idx)
        val obj = x.newInstance().asInstanceOf[JdbcValueMapper[AnyRef]]
        obj.getBeanValue(rs.getObject(idx), x.asInstanceOf[Class[AnyRef]])

      case x if jdbcValueMapperFactory.getJdbcValueMapper(x) != null =>
        val mapper = jdbcValueMapperFactory.getJdbcValueMapper(x).asInstanceOf[JdbcValueMapper[AnyRef]]
        mapper.getBeanValue(rs.getObject(idx), x.asInstanceOf[Class[AnyRef]])
    }
  }

  def insert[T](conn: RichConnection, entity: T): Unit = new BeanOperation[T](entity)(conn.jdbcValueMapperFactory).insert(conn)
  def insert[T](dataSource: RichDataSource, entity: T): Unit = dataSource.withConnection { conn =>
    new BeanOperation[T](entity)(dataSource.jdbcValueMapperFactory).insert(new RichConnection(conn)(dataSource.jdbcValueMapperFactory))
  }

  def update[T](conn: RichConnection, entity: T): Unit =
    new BeanOperation[T](entity)(conn.jdbcValueMapperFactory).update(conn)

  def update[T](dataSource: RichDataSource, entity: T): Unit = dataSource.withConnection { conn =>
    new BeanOperation[T](entity)(conn.jdbcValueMapperFactory).update(new RichConnection(conn)(dataSource.jdbcValueMapperFactory))
  }

  def delete[T](conn: RichConnection, entity: T): Unit =
    new BeanOperation[T](entity)(conn.jdbcValueMapperFactory).delete(conn)

  def delete[T](dataSource: RichDataSource, entity: T): Unit = dataSource.withConnection { conn =>
    new BeanOperation[T](entity)(conn.jdbcValueMapperFactory).delete(new RichConnection(conn)(dataSource.jdbcValueMapperFactory))
  }

  // TODO
  private def bean2Row(bean: AnyRef): Row = ???

  // TODO
  private def row2Bean[T: Manifest](row: Row): T = ???
}

trait BeanMapping[E] {

  trait FieldMapping[F] {
    val fieldName: String
    val columnName: String
    val fieldType: Class[F]

    val isId: Boolean
    val isAutoIncrement: Boolean

    def get(bean: E): F
    def set(bean: E, value: F): Unit

    override def toString(): String = s"Field(field=$fieldName, column=$columnName)"
  }

  val reflectClass: Class[E]
  val catelog: String
  val tableName: String
  val camelToUnderscore: Boolean
  val fields: List[FieldMapping[_]]
  val idFields: List[FieldMapping[_]]
  
  def getFieldByName(name: String): Option[FieldMapping[_]]
  def getFieldByColumnName(columnName:String): Option[FieldMapping[_]]

}


class UnionBeanMapping[E](val reflectClass: Class[E])(jdbcValueMapperFactory: JdbcValueMapperFactory) extends BeanMapping[E] {

  trait TmpFieldMapping[F] extends FieldMapping[F] {
    val isTransient: Boolean
  }
  val antTable = reflectClass.getAnnotation(classOf[Table])
  val catelog = if (antTable != null) antTable.catelog() else ""
  val tableName = if (antTable != null && antTable.value() != "") antTable.value()
    else reflectClass.getSimpleName.toLowerCase
  val camelToUnderscore = if(antTable != null) antTable.camelToUnderscore() else false

  val fields = getMappingFields
  val idFields = fields.filter(_.isId)
  val fieldsByName: Map[String, FieldMapping[_]] = fields.map { field=>
    (field.fieldName, field)
  }.toMap
  val fieldsByColumnName: Map[String, FieldMapping[_]] = fields.map { field =>
    (field.columnName, field)
  }.toMap

  private def getAnnotation[T <: Annotation](annotationType: Class[T], getter: Method, setter: Method, fall: java.lang.reflect.Field): T =
    if (getter.isAnnotationPresent(annotationType)) getter.getAnnotation(annotationType)
    else if (setter.isAnnotationPresent(annotationType)) setter.getAnnotation(annotationType)
    else if(fall != null && fall.isAnnotationPresent(annotationType)) fall.getAnnotation(annotationType)
    else null.asInstanceOf[T]

  private def newFieldMapping[T](name: String, getter: Method, setter: Method, fallField: java.lang.reflect.Field): TmpFieldMapping[T] = new TmpFieldMapping[T] {
    val antColumn = getAnnotation(classOf[Column], getter, setter, fallField)
    val antId = getAnnotation(classOf[Id], getter, setter, fallField)
    val fieldType = getter.getReturnType.asInstanceOf[Class[T]]
    val fieldName = name
    val columnName = if (antColumn != null && antColumn.name != "") antColumn.name
      else if(camelToUnderscore) BeanMapping.mappingCamelToUnderscore(fieldName)
      else fieldName
    val isTransient = if(antColumn != null ) antColumn.isTransient else false
    val isId = antId != null
    val isAutoIncrement = (antId != null && antId.auto)

    def get(bean: E) = getter.invoke(bean).asInstanceOf[T]
    def set(bean: E, value: T) {
      setter.invoke(bean, value.asInstanceOf[AnyRef])

    }
  }



  def getFieldByName(name: String) = fieldsByName.get(name)
  def getFieldByColumnName(columnName:String) = fieldsByColumnName.get(columnName)

  /**
   * support 2 styles mapping:
   * 1. scala style. eg: name() for getter and name_=(arg) for setter
   * 2. JavaBean Style. eg: getName()/isName() setName()
   */
  def getMappingFields: List[FieldMapping[_]] = {

    def isSupportedDataType(cls: Class[_]) =
      BeanMapping.isSupportedDataType(cls) || jdbcValueMapperFactory.getJdbcValueMapper(cls) != null

    val getters: Map[String, Method] = reflectClass.getMethods.filter { method =>
      method.getParameterTypes.length == 0 && isSupportedDataType(method.getReturnType)
    }.map { method=> (method.getName, method)}.toMap

    val setters: Map[String, Method] = reflectClass.getMethods.filter { method =>
      method.getParameterTypes.length == 1 && isSupportedDataType(method.getParameterTypes.apply(0)) && method.getReturnType == Void.TYPE
    }.map{ method=> (method.getName, method)}.toMap

    // 2015-09-14 scan for class hierarchy
    def getField(name: String): java.lang.reflect.Field = {
      var fromClass: Class[_] = reflectClass
      var field: java.lang.reflect.Field = null

      while(field == null && fromClass != null) {
        field =
          try { fromClass.getDeclaredField(name) }
          catch { case ex: Throwable => null }

        fromClass = fromClass.getSuperclass
      }

      field
    }

    // Name -> name
    def normaliPropertyName(name: String) = name.charAt(0).toLower.toString + name.substring(1)

    val mappings: Iterable[TmpFieldMapping[_]] = getters.keys.flatMap { name =>

      // style: name(), name_=(arg)
      val scala = for( getter <- getters.get(name);
        setter <- setters.get(name + "_$eq");
        if(getter.getReturnType == setter.getParameterTypes.apply(0))
      ) yield newFieldMapping(name, getter, setter, getField(name))

      // style: isName() setName(arg)
      val is = for( getter <- getters.get(name) if name.startsWith("is") && getter.getReturnType == classOf[Boolean];
        setter <- setters.get("set" + name.substring(2));
        if(getter.getReturnType == setter.getParameterTypes.apply(0))
      ) yield newFieldMapping(normaliPropertyName(name.substring(2)), getter, setter, getField(name))

      // style: getName() setName(arg)
      val get = for( getter <- getters.get(name) if name.startsWith("get") ;
           setter <- setters.get("set" + name.substring(3));
           if(getter.getReturnType == setter.getParameterTypes.apply(0))
      ) yield newFieldMapping(normaliPropertyName(name.substring(3)), getter, setter, getField(name))

      //
      scala.orElse(is).orElse(get)
    }

    mappings.toList.filter( _.isTransient == false )
      .groupBy(_.fieldName).map(_._2.apply(0)).toList   // avoid field dupicate, such as name/name_= and getName/setName

  }

}

class BeanOperation[T](bean: T)(implicit jdbcValueMapperFactory: JdbcValueMapperFactory) {

  val beanMapping = BeanMapping.getBeanMapping(bean.getClass.asInstanceOf[Class[T]])(jdbcValueMapperFactory)
  val includeColumns = collection.mutable.ListBuffer[String]() ++ beanMapping.fields.map(_.columnName)
  val allColumns = beanMapping.fields.map(_.columnName)

  var ignoreNullField = false
  var ignoreEmptyField = false
  val qualifiedTableName =
    (if (beanMapping.catelog != null && beanMapping.catelog != "") beanMapping.catelog + "." else "") +
    beanMapping.tableName

  def includeColumn(names: String*): this.type = {
    val toBeAdd = names.filter(allColumns contains _).filterNot(includeColumns contains _)
    includeColumns ++= toBeAdd
    this
  }

  def excludeColumn(names: String*): this.type = {
    val toBeRemove = names.filter(allColumns contains _).filter(includeColumns contains _)
    includeColumns --= toBeRemove
    this
  }

  def ignoreEmptyField(flag: Boolean = true): this.type  = {
    ignoreEmptyField = flag
    this
  }

  def ignoreNullField(flag: Boolean = true): this.type  = {
    ignoreNullField = flag
    this
  }


  def isNull(t: Any): Boolean = t == null || t == None || t == Nil
  def isEmpty(t: Any): Boolean = isNull(t) ||
    (t match {
      case n : Number => n == 0
      case "" => true
      case _ => false
    })

  lazy val hasId: Boolean = {
    val idColumns = beanMapping.idFields

    !idColumns.isEmpty && idColumns.forall { fieldMapping =>
      val value = fieldMapping.get(bean)

      (value != null) && (fieldMapping.fieldType match {
        case java.lang.Integer.TYPE | ClassOfInteger | java.lang.Short.TYPE | ClassOfShort |
             java.lang.Long.TYPE | ClassOfLong  =>
          value.asInstanceOf[Number].longValue != 0
        case ClassOfBigDecimal =>
          value.asInstanceOf[java.math.BigDecimal].longValue != 0
        case ClassOfScalaBigDecimal =>
          value.asInstanceOf[scala.math.BigDecimal].longValue != 0
        case _ => true
      })
    }
  }

  def insert(conn: RichConnection): Unit = {
    val insertFields = beanMapping.fields.filter(x => includeColumns contains x.columnName)
      .filterNot( x => ignoreNullField && isNull(x.get(bean)) )
      .filterNot( x => ignoreEmptyField && isEmpty(x.get(bean)) )

    // check the bean hasId setting
    val idColumns = beanMapping.idFields

    if (!hasId) { // try auto generate
      val fields = insertFields.filterNot(_.isId)

      val sql = "insert into " + qualifiedTableName + "(" + fields.map(_.columnName).mkString(",") +
        ") values (" + fields.map(_ => '?').mkString(",") + ")"

      val args = fields.map(_.get(bean)).toSeq

      conn.executeUpdateWithGenerateKey(SQLWithArgs(sql, args)) { rs =>
        if (rs.next) {
          idColumns.foreach { col =>
            val value = col.fieldType match {
              case java.lang.Boolean.TYPE | ClassOfBoolean => rs.getBoolean(1)
              case java.lang.Integer.TYPE | ClassOfInteger => rs.getInt(1)
              case _ => throw new AssertionError
            }
            col.asInstanceOf[beanMapping.FieldMapping[Any]].set(bean, value)
          }
        }
      }
    } else {  // hasID

      val fields = insertFields
      val sql = "insert into " + qualifiedTableName + "(" + fields.map(_.columnName).mkString(",") +
        ") values (" + fields.map(_ => '?').mkString(",") + ")"

      val args = fields.map(_.get(bean)).toSeq
      conn.executeUpdate(SQLWithArgs(sql, args))
    }
  }

  def update(conn: RichConnection): Unit = {
    // if has Id field and with value
    val idColumns = beanMapping.idFields

    if (!hasId) throw new AssertionError("bean's id field is not setted")

    val ids = beanMapping.fields.filter(_.isId)

    val fields = beanMapping.fields.filterNot(_.isId)
      .filter(x  => includeColumns contains x.columnName)
      .filterNot(x => ignoreNullField && isNull(x.get(bean)) )
      .filterNot(x => ignoreEmptyField && isEmpty(x.get(bean)) )

    val sql = "update " + qualifiedTableName + " set " + fields.map(x => x.columnName + " = ?").mkString(",") +
      " where " + ids.map( x => x.columnName + " = ?").mkString(" and ")

    val args = fields.map(_.get(bean)).toSeq ++ ids.map(_.get(bean)).toSeq

    conn.executeUpdate(SQLWithArgs(sql, args))
  }


  def delete(conn: RichConnection): Unit = {

    if (!hasId) throw new AssertionError("bean's id field is not setted")

    val ids = beanMapping.fields.filter(_.isId)
    val sql = "delete from " + qualifiedTableName + " where " +
      ids.map( x => x.columnName + " = ? ").mkString(" and ")

    val args = ids.map(_.get(bean)).toSeq

    conn.executeUpdate(SQLWithArgs(sql, args))
  }

  def insert(dataSource: RichDataSource): Unit = dataSource.withConnection { conn =>
    val rich = new RichConnection(conn)(dataSource.jdbcValueMapperFactory)
    insert(rich)
  }

  def update(dataSource: RichDataSource): Unit = dataSource.withConnection { conn =>
    val rich = new RichConnection(conn)(dataSource.jdbcValueMapperFactory)
    update(rich)
  }

  def delete(dataSource: RichDataSource): Unit = dataSource.withConnection { conn =>
    val rich = new RichConnection(conn)(dataSource.jdbcValueMapperFactory)
    delete(rich)
  }


}

