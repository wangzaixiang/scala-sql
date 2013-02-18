package wangzx.scala_commons.sql

import scala.collection.mutable.ListBuffer
import java.lang.reflect.Field
import java.lang.reflect.Modifier
import scala.collection.mutable.MapBuilder
import java.lang.reflect.Method
import java.lang.annotation.Annotation
import java.beans.Introspector
import java.beans.PropertyDescriptor
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

  val G_BeanMappings = collection.mutable.Map[Class[_], SoftReference[BeanMapping[_]]]()

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
    case _ => false
  }


  def getBeanMapping[T](clazz: Class[T]): BeanMapping[T] = {
    synchronized {
      val cached = G_BeanMappings.get(clazz)
      cached match {
        case Some(x) => x.get match {
          case Some(result) => return result.asInstanceOf[BeanMapping[T]]
          case None =>
        }
        case _ =>
      }
      val realClass = real_class(clazz)
      val classOfScalaObject = classOf[ScalaObject]
      
      val mapping = if(classOfScalaObject.isAssignableFrom(realClass)) new ScalaBeanMapping(realClass)
      	else new JavaBeanMapping(realClass)
      
      G_BeanMappings(clazz) = new SoftReference(mapping)
      return mapping.asInstanceOf[BeanMapping[T]]
    }
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
  val fields: List[FieldMapping[_]]
  val idFields: List[FieldMapping[_]]
  
  def getFieldByName(name: String): Option[FieldMapping[_]]
  def getFieldByColumnName(columnName:String): Option[FieldMapping[_]]

}

abstract class BaseBeanMapping[E](val reflectClass: Class[E]) extends BeanMapping[E] {
  import BeanMapping._

  val realClass = real_class(reflectClass)
  val antTable = realClass.getAnnotation(classOf[Table])
  val catelog = if (antTable != null) antTable.catelog() else ""
  val tableName = if (antTable != null && antTable.value() != "") antTable.value()
  else realClass.getSimpleName.toLowerCase

  val fields = getMappingFields
  val idFields = fields.filter(_.isId)
  val fieldsByName: Map[String, FieldMapping[_]] = fields.map { field=>
    (field.fieldName, field)
  }.toMap
  val fieldsByColumnName: Map[String, FieldMapping[_]] = fields.map { field =>
    (field.columnName, field)
  }.toMap

  private def getAnnotation[T <: Annotation](annotationType: Class[T], getter: Method, setter: Method): T =
    if (getter.isAnnotationPresent(annotationType)) getter.getAnnotation(annotationType)
    else if (setter.isAnnotationPresent(annotationType)) setter.getAnnotation(annotationType)
    else null.asInstanceOf[T]

  def newFieldMapping[T](name: String, getter: Method, setter: Method): FieldMapping[T] = new FieldMapping[T] {
    val antColumn = getAnnotation(classOf[Column], getter, setter)
    val antId = getAnnotation(classOf[Id], getter, setter)
    val fieldType = getter.getReturnType.asInstanceOf[Class[T]]
    val fieldName = name
    val columnName = if (antColumn != null && antColumn.name != "") antColumn.name else fieldName
    val isId = antId != null
    val isAutoIncrement = (antId != null && antId.auto)

    def get(bean: E) = getter.invoke(bean).asInstanceOf[T]
    def set(bean: E, value: T) {
      setter.invoke(bean, value.asInstanceOf[AnyRef])
    }
  }

  def getMappingFields: List[FieldMapping[_]]
  def getFieldByName(name: String) = fieldsByName.get(name)
  def getFieldByColumnName(columnName:String) = fieldsByColumnName.get(columnName)

}

class ScalaBeanMapping[E](override val reflectClass: Class[E]) extends BaseBeanMapping(reflectClass) {
  import BeanMapping._

  def getMappingFields: List[FieldMapping[_]] = {
    val methods = realClass.getMethods()

    methods.flatMap { method =>
      if (method.getParameterTypes().length == 0 && isSupportedDataType(method.getReturnType())) {
        val setterName = method.getName() + "_$eq"
        try {
          val setter = realClass.getMethod(setterName, method.getReturnType)
          val field = newFieldMapping(method.getName, method, setter)
          Some(field)
        } catch {
          case ex: Throwable => None
        }
      }
      else None
    }.toList

  }

}

class JavaBeanMapping[E](override val reflectClass: Class[E]) extends BaseBeanMapping(reflectClass) {
  import BeanMapping._
  
  def getMappingFields: List[FieldMapping[_]] = {
    Introspector.getBeanInfo(realClass).getPropertyDescriptors.flatMap { pd =>
      if(pd.getReadMethod() != null && pd.getWriteMethod() != null && isSupportedDataType(pd.getPropertyType()))
    	Some(newFieldMapping(pd.getName, pd.getReadMethod, pd.getWriteMethod))
      else None
    }.toList
  }
    
  
}