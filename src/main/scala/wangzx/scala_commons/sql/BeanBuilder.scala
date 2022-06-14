package wangzx.scala_commons.sql

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable
import scala.language.experimental.macros

/**
  * TODO support camel and underscore name mapping like doSomething <-> do_something
  */
object BeanBuilder {

  def build[T](sources: AnyRef*)(additions: (String, Any)*): T = ???

  def build[T](sources: AnyRef*)(additions: T=>T = identity): T = ???

}
