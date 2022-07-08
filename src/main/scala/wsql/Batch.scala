package wsql

import java.sql.{Connection, PreparedStatement, SQLType, Types}
import scala.language.experimental.macros

trait Batch[T] {

  var autoCommitCount : Int = 256

  def addBatch(value: T): Unit

  def commit(): Unit

  def close(): Unit

}


case class BatchImpl[T](conn: Connection, statement: String ) ( proc: T => List[JdbcValue[?]|Null] ) extends Batch[T] {

  val stmt: PreparedStatement = conn.prepareStatement(statement).nn

  var toBeCommit = 0

  def addBatch(value: T): Unit = {
    val args: Seq[JdbcValue[?]|Null] = proc(value)
    var idx = 1

    while( idx <= args.size ){
      val para = args(idx-1)
      if(para == null) stmt.setNull(idx, Types.CHAR)
      else para.passIn(stmt, idx)

      idx += 1
    }
    stmt.addBatch()
    toBeCommit += 1
    if(toBeCommit >= autoCommitCount) {
      stmt.executeBatch()
      toBeCommit = 0
    }

  }

  override def commit(): Unit = {
    if(toBeCommit > 0) {
      stmt.executeBatch()
      toBeCommit = 0
    }
  }

  override def close(): Unit = {
    commit()
    stmt.close()
  }


}