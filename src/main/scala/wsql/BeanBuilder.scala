package wsql

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable
import scala.reflect.ClassTag
import scala.language.experimental.macros

/**
  * TODO support camel and underscore name mapping like doSomething <-> do_something
  */
object BeanBuilder {

  inline def build[T:deriving.Mirror.ProductOf](inline sources: AnyRef*): T =
    ${ Macros.buildCaseClassImpl[T]('sources)('{Seq.empty}) }

  inline def build[T:deriving.Mirror.ProductOf](inline sources: AnyRef*)(inline additions: (String, Any)*): T =
    ${ Macros.buildCaseClassImpl[T]('sources)('additions) }

// TODO
//  inline def build[T:deriving.Mirror.ProductOf](sources: AnyRef*)(additions: T=>T = identity): T =
//    ${ Macros.buildImpl2[T]('sources)('additions) }

  /**
   * if you need convert from A[X] => B[Y], you need import BeanBuilder.CollectionConverts.given
   */
  object CollectionConverts:
    given [T]: Conversion[ Seq[T], List[T] ] with
      def apply(src: Seq[T]): List[T] = src.toList

    given [T: ClassTag]: Conversion[ Seq[T], Array[T]] with
      def apply(src: Seq[T]): Array[T] = src.toArray

    given [T: ClassTag]: Conversion[ Array[T], Seq[T] ] with
      def apply(src: Array[T]): Seq[T] = src.toSeq

    given [T: ClassTag]: Conversion[ List[T], Array[T]] with
      def apply(src: List[T]): Array[T] = src.toArray

    given [T: ClassTag]: Conversion[ Array[T], List[T] ] with
      def apply(src: Array[T]): List[T] = src.toList
}