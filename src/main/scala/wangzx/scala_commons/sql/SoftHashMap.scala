package wangzx.scala_commons.sql

import java.util.concurrent.ConcurrentHashMap

import wangzx.scala_commons.sql.SoftMap.SoftValue

import scala.collection.mutable.{Map, HashMap}
import java.lang.ref._

object SoftMap {

  class SoftKey[K](key: K, val parent: SoftMap[K,_])
    extends SoftReference(key, queue.asInstanceOf[ReferenceQueue[K]]) {

    val hash = key.hashCode()

    override def equals(other: Any): Boolean =  other match {
      case null => false
      case o: AnyRef if o eq this => true
      case o: SoftKey[_] =>
        val myKey = this.get
        val oKey = o.get
        if(myKey != null && myKey == oKey) true
        else false
    }

    override def hashCode = hash

    def removeFromParent(): Unit = {
      parent.internalRemove(this)
    }

  }

  class SoftValue[K, V <: AnyRef](val key: SoftKey[K], value: V, val parent: SoftMap[K, V])
    extends SoftReference(value, queue.asInstanceOf[ReferenceQueue[V]]) {

    def removeFromParent(): Unit = {
      parent.internalRemove(key)
    }

  }

  val queue = new ReferenceQueue[AnyRef]

  new Thread("SoftMap-Cleaner") {

    this.setDaemon(true)

    override def run(): Unit = {
      while (true) {
        queue.remove match {
          case sv: SoftValue[_, _] =>
            sv.removeFromParent()
          case sk: SoftKey[_] =>
            sk.removeFromParent()
          case _ =>
        }
      }
    }

  }.start


}

class SoftMap[K, V <: AnyRef] extends Map[K, V] {

  private val map = new ConcurrentHashMap[SoftMap.SoftKey[K], SoftMap.SoftValue[K, V]]

  private def internalRemove(key: SoftMap.SoftKey[K]): Option[V] = {
    //println("remove key")
    map.remove(key) match {
      case null => None
      case sv => sv.get match {
        case null => None
        case v => Some(v)
      }
    }
  }

  override def +=(kv: (K, V)): this.type = {
    val sk = new SoftMap.SoftKey[K](kv._1, this)
    val sv = new SoftMap.SoftValue[K,V](sk, kv._2, this)
    map.put(sk, sv)
    this
  }

  override def get(key: K): Option[V] = map.get(new SoftMap.SoftKey(key, null)) match {
    case sv: SoftValue[K, V] =>
      sv.get match {
        case v if v != null => Some(v)
        case null => None
      }
    case null => None
  }


  override def -=(key: K): this.type = {
    map.remove(new SoftMap.SoftKey(key, null))
    this
  }

  override def iterator: Iterator[(K, V)] = {
    import scala.collection.JavaConversions._
    map.iterator.flatMap { case (sk, sv) =>
      val key = sk.get
      val value = sv.get
      if(key != null && value != null) Some(key, value)
      else None
    }
  }

  override def empty: SoftMap[K, V] = new SoftMap[K, V]

  override def size = map.size
}
