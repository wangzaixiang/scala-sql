package wangzx.scala_commons.sql

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable
import scala.language.experimental.macros

/**
  * TODO support camel and underscore name mapping like doSomething <-> do_something
  */
object BeanBuilder {

  inline def build[T](sources: AnyRef*)(additions: (String, Any)*): T =
    ${ Macros.buildImpl1[T]('sources)('additions) }

  inline def build[T](sources: AnyRef*)(additions: T=>T = identity): T =
    ${ Macros.buildImpl2[T]('sources)('additions) }

}
