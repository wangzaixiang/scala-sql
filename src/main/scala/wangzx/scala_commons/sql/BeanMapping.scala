package wangzx.scala_commons.sql

import java.lang.reflect.Method
import java.lang.annotation.Annotation
import java.beans.Introspector
import scala.ref.SoftReference

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
    case _ => classOf[JdbcValue].isAssignableFrom(typ)
  }

  def getBeanMapping[T](clazz: Class[T]): BeanMapping[T] = {
    synchronized {
      val cached: Option[BeanMapping[_]] = G_BeanMappings.get(clazz)
      cached match {
        case Some(result) =>
          result.asInstanceOf[BeanMapping[T]]

        case None =>
          val realClass = real_class(clazz)
          val mapping = new UnionBeanMapping(realClass)
          G_BeanMappings(clazz) = mapping
          return mapping.asInstanceOf[BeanMapping[T]]
      }
    }
  }

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


class UnionBeanMapping[E](val reflectClass: Class[E]) extends BeanMapping[E] {

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

    val getters: Map[String, Method] = reflectClass.getMethods.filter { method =>
      method.getParameterTypes.length == 0 && BeanMapping.isSupportedDataType(method.getReturnType)
    }.map { method=> (method.getName, method)}.toMap

    val setters: Map[String, Method] = reflectClass.getMethods.filter { method =>
      method.getParameterTypes.length == 1 && BeanMapping.isSupportedDataType(method.getParameterTypes.apply(0)) &&
        method.getReturnType == Void.TYPE
    }.map{ method=> (method.getName, method)}.toMap

    def getField(name: String): java.lang.reflect.Field =
      try { reflectClass.getDeclaredField(name) }
      catch { case ex => null }

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
      ) yield newFieldMapping(name.substring(2), getter, setter, getField(name))

      // style: getName() setName(arg)
      val get = for( getter <- getters.get(name) if name.startsWith("get") ;
           setter <- setters.get("set" + name.substring(3));
           if(getter.getReturnType == setter.getParameterTypes.apply(0))
      ) yield newFieldMapping(name.substring(3), getter, setter, getField(name))

      //
      scala.orElse(is).orElse(get)
    }

    mappings.toList.filter( _.isTransient == false )

  }

}