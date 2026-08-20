package wsql

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable
import scala.reflect.ClassTag
import wsql.macros.BuilderMacros

/**
  * BeanBuilder provides compile-time safe object transformation utilities for Scala 3 case classes.
  * 
  * It uses macros to generate efficient transformation code at compile time, eliminating runtime reflection
  * overhead while maintaining type safety. The builder supports complex type conversions, nested objects,
  * collection transformations, and Option type handling.
  * 
  * == Features ==
  *  - '''Compile-time safety''': All type checking and code generation happens at compile time
  *  - '''Zero runtime overhead''': Generated code directly calls constructors without reflection
  *  - '''Intelligent type conversion''': Supports implicit conversions, Option handling, and collection transformations
  *  - '''Nested object support''': Recursively transforms nested case classes
  *  - '''Flexible field mapping''': Maps fields by name with customizable precedence rules
  * 
  * == Basic Usage ==
  * {{{
  * case class PersonA(name: String, age: Int)
  * case class PersonB(name: String, age: Int, status: String = "active")
  * 
  * val personA = PersonA("John", 30)
  * val personB = BeanBuilder.build[PersonB](personA)  // PersonB("John", 30, "active")
  * }}}
  * 
  * == Advanced Usage ==
  * {{{
  * // Complex type conversions with custom implicit conversions
  * given Conversion[String, Int] = Integer.parseInt
  * case class Source(name: String, age: String, tags: Seq[String])
  * case class Target(name: String, age: Option[Int], tags: List[String])
  * 
  * import BeanBuilder.CollectionConverters.given
  * val source = Source("Alice", "25", Seq("admin", "user"))
  * val target = BeanBuilder.build[Target](source)
  * // Target("Alice", Some(25), List("admin", "user"))
  * }}}
  * 
  * == Field Resolution Priority ==
  * 1. `additions` parameter values (highest priority)
  * 2. Source object fields (matched by name, at most 1 source, otherwise need specified in additions)
  * 3. Target case class default parameter values
  * 4. Option[T] fields default to None (lowest priority)
  * 
  * == Best Practices ==
  *  - Use meaningful field names that match across source and target types
  *  - Import `CollectionConverters.given` when working with different collection types
  *  - Define custom `given Conversion[A, B]` instances for domain-specific transformations
  *  - Prefer immutable case classes for both source and target types
  *  - Use default parameters in target case classes to handle missing fields gracefully
  * 
  * @note Target type must be a case class with `deriving.Mirror.ProductOf` support
  * @note Self-referencing case classes are not currently supported, TODO add self-referencing in future version.
  * TODO support camel and underscore name mapping like doSomething <-> do_something
  */
object BeanBuilder {

  /**
    * Builds a target case class instance from one or more source objects.
    * 
    * Fields are mapped by name from source objects to the target type. The builder applies
    * intelligent type conversion rules including implicit conversions, Option wrapping/unwrapping,
    * collection transformations, and recursive case class conversion.
    * 
    * @tparam T Target case class type (must have deriving.Mirror.ProductOf)
    * @param sources Source objects to extract field values from. Fields with the same name
    *                will be mapped from sources to target. If multiple sources contain the
    *                same field name, the last one takes precedence.
    * @return New instance of type T with fields populated from sources
    * 
    * @example Multiple source objects:
    * {{{
    * case class Source1(name: String, value: Int)
    * case class Source2(age: Int, active: Boolean)  
    * case class Target(name: String, age: Int, value: Int, active: Boolean)
    * 
    * val src1 = Source1("test", 42)
    * val src2 = Source2(25, true)
    * val result = BeanBuilder.build[Target](src1, src2)
    * // Target("test", 25, 42, true)
    * }}}
    * 
    * @example Type conversions:
    * {{{
    * case class Source(name: String, age: String)
    * case class Target(name: String, age: Option[Int])
    * 
    * given Conversion[String, Int] = Integer.parseInt
    * 
    * val source = Source("Alice", "30")
    * val target = BeanBuilder.build[Target](source)
    * // Target("Alice", Some(30))
    * }}}
    * 
    * @note The transformation rules applied in order of precedence:
    *       1. Direct type match
    *       2. Implicit Conversion[Source, Target]
    *       3. Option wrapping (A => Option[B])
    *       4. Option unwrapping (Option[A] => B)  
    *       5. Collection element mapping (List[A] => List[B])
    *       6. Collection type conversion (List[A] => Array[B])
    *       7. Recursive case class conversion
    */
  inline def build[T:deriving.Mirror.ProductOf](inline sources: AnyRef*): T =
    ${ BuilderMacros.buildCaseClassImpl[T]('sources)('{Seq.empty}) }

  /**
    * Builds a target case class instance from source objects with additional field overrides.
    * 
    * This version allows you to override or supplement field values using the additions parameter.
    * Addition values take highest precedence over source object fields and default values.
    * 
    * @tparam T Target case class type (must have deriving.Mirror.ProductOf)
    * @param sources Source objects to extract field values from
    * @param additions Additional field assignments as (fieldName -> value) tuples.
    *                  These values override any matching fields from source objects.
    * @return New instance of type T with fields populated from sources and additions
    * 
    * @example Field override:
    * {{{
    * case class Source(name: String, age: Int)
    * case class Target(name: String, age: Int, status: String = "inactive")
    * 
    * val source = Source("Alice", 25)
    * val target = BeanBuilder.build[Target](source)("age" -> 30, "status" -> "active")
    * // Target("Alice", 30, "active")
    * }}}
    * 
    * @example Providing missing fields:
    * {{{
    * case class Source(name: String)
    * case class Target(name: String, age: Int, country: String)
    * 
    * val source = Source("Bob")
    * val target = BeanBuilder.build[Target](source)("age" -> 25, "country" -> "USA")
    * // Target("Bob", 25, "USA")
    * }}}
    * 
    * @note Field value resolution priority:
    *       1. additions parameter (highest)
    *       2. source object fields (by name match)
    *       3. case class default values
    *       4. Option[T] defaults to None (lowest)
    */
  inline def build[T:deriving.Mirror.ProductOf](inline sources: AnyRef*)(inline additions: (String, Any)*): T =
    ${ BuilderMacros.buildCaseClassImpl[T]('sources)('additions) }

// TODO
//  inline def build[T:deriving.Mirror.ProductOf](sources: AnyRef*)(additions: T=>T = identity): T =
//    ${ Macros.buildImpl2[T]('sources)('additions) }

  /**
   * CollectionConverters provides implicit conversions between common Scala collection types.
   * 
   * These converters are particularly useful when transforming objects with collection fields
   * of different types. Import the given instances to enable automatic collection conversions
   * during BeanBuilder transformations.
   * 
   * @example Usage:
   * {{{
   * import BeanBuilder.CollectionConverters.given
   * 
   * case class Source(items: Seq[String])
   * case class Target(items: List[String])
   * 
   * val source = Source(Seq("a", "b", "c"))
   * val target = BeanBuilder.build[Target](source)  // List conversion applied automatically
   * // Target(List("a", "b", "c"))
   * }}}
   * 
   * @note For Array conversions, the element type must have a ClassTag for runtime type information.
   */
  object CollectionConverters:
    
    /**
     * Converts Seq[T] to List[T].
     * 
     * @tparam T Element type
     */
    given [T]: Conversion[ Seq[T], List[T] ] with
      def apply(src: Seq[T]): List[T] = src.toList

    /**
     * Converts Seq[T] to Array[T].
     * 
     * @tparam T Element type (requires ClassTag for Array creation)
     */
    given [T: ClassTag]: Conversion[ Seq[T], Array[T]] with
      def apply(src: Seq[T]): Array[T] = src.toArray

    /**
     * Converts Array[T] to Seq[T].
     * 
     * @tparam T Element type (requires ClassTag for Array access)
     */
    given [T: ClassTag]: Conversion[ Array[T], Seq[T] ] with
      def apply(src: Array[T]): Seq[T] = immutable.ArraySeq.unsafeWrapArray(src)

    /**
     * Converts List[T] to Array[T].
     * 
     * @tparam T Element type (requires ClassTag for Array creation)
     */
    given [T: ClassTag]: Conversion[ List[T], Array[T]] with
      def apply(src: List[T]): Array[T] = src.toArray

    /**
     * Converts Array[T] to List[T].
     * 
     * @tparam T Element type (requires ClassTag for Array access)
     * @note Custom implementation to handle potential Array-specific issues
     */
    given [T: ClassTag]: Conversion[ Array[T], List[T] ] with
      def apply(src: Array[T]): List[T] =
          val builder = collection.mutable.ListBuffer[T]()
          var i = 0
          while i < src.length do
              builder.append(src(i))
              i += 1
          builder.toList
}