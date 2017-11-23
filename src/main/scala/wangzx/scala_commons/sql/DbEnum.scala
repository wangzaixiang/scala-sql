package wangzx.scala_commons.sql

import java.sql.{PreparedStatement, ResultSet}

/**
 *
 */
trait DbEnum {

  val id: Int
  val name: String

  override def toString: String = s"$id:$name"
  override def hashCode() = id

  override def equals(obj: scala.Any): Boolean =
    if(obj == null) false
    else if(obj == this) true
    else if(getClass.isInstance(obj)) obj.asInstanceOf[DbEnum].id == this.id
    else false

}

class DbEnumJdbcValueAccessor[T <: DbEnum](valueOf: Int => T) extends JdbcValueAccessor[T] {
    override def passIn(stmt: PreparedStatement, index: Int, value: T): Unit = stmt.setInt(index, value.id)

    override def passOut(rs: ResultSet, index: Int) = valueOf( rs.getInt(index) )

    override def passOut(rs: ResultSet, name: String): T = valueOf( rs.getInt(name) )
}