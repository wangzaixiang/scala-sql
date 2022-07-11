package wsql_test

import org.scalatest.funsuite.AnyFunSuite
import wsql.BeanBuilder

class BuilderTest extends AnyFunSuite {

  test("basic builder") {

    case class PersonA(name: String, age: Int, address: AddressA)
    case class AddressA(street: String, city: String)

    case class PersonB(name: String, age: Int, address: AddressB)
    case class AddressB(street: String, city: String)

    val personA = PersonA("John", 30, AddressA("123 Main St", "Anytown"))
    val personB = PersonB("John", 30, AddressB("123 Main St", "Anytown"))

    assert( BeanBuilder.build[PersonB](personA) == personB )
    assert( BeanBuilder.build[PersonA](personB) == personA )

  }

  test("import conversions") {
    given Conversion[String, Int] with
      override def apply(x: String): Int = Integer.parseInt(x)
    given Conversion[Int, String] with
      override def apply(x: Int): String = Integer.toString(x)

    case class PersonA(name: String, age: String, address: AddressA)
    case class AddressA(street: String, city: String)

    case class PersonB(name: String, age: Option[Int], address: AddressB)
    case class AddressB(street: String, city: String)

    val personA = PersonA("John", null, AddressA("123 Main St", "Anytown"))
    val personB = PersonB("John", None, AddressB("123 Main St", "Anytown"))

    assert( BeanBuilder.build[PersonB](personA) == personB ) // // TODO same field must convert, don't use default
    assert( BeanBuilder.build[PersonA](personB) == personA )
  }

  test("Collection fields") {

    import BeanBuilder.CollectionConverters.given

    // X[A] => X[B] import givens A=>B  or CaseClass(A) => CaseClass(B)
    // X[A] => Y[A] impor givens X[?] => Y[?]
    // X[A] => Y[B] import A=>B, X=>Y
    case class Person2A(name: String, address: Seq[Address2A])
    case class Address2A(street: String, city: String)

    case class Person2B(name: String, age: Int = 0, address: List[Address2B])
    case class Address2B(street: String, city: String = "**")

    val person2A = Person2A("John", Seq( Address2A("123 Main St", "Anytown") ))
    val person2B = Person2B("John", 0, List( Address2B("123 Main St", "Anytown") ))

    assert( BeanBuilder.build[Person2B](person2A) == person2B )
    assert( BeanBuilder.build[Person2B](person2A) == person2B )

  }

  test("option fields") {

    // String -> Int
    // String -> Option[String]
    // String -> Option[Int]

    case class Person3A(name: String, address: Address3A)
    case class Address3A(street: String, city: String)

    case class Person3B(name: String, age: Int = 0, address: Option[Address3B])
    case class Address3B(street: String, city: String = "**")

    val person3A = Person3A("John",  Address3A("123 Main St", "Anytown") )
    val person3B = Person3B("John", 0, Some( Address3B("123 Main St", "Anytown") ))
    assert( BeanBuilder.build[Person3B](person3A) == person3B )
    assert( BeanBuilder.build[Person3A](person3B) == person3A )

  }

  test("additional fields"){
    case class Person3A(name: String, address: Address3A)
    case class Address3A(street: String, city: String)

    case class Person3B(name: String, age: Int = 0, address: Option[Address3B])
    case class Address3B(street: String, city: String = "**", country: String)

    given Conversion[Int, String] with
      override def apply(x: Int): String = Integer.toString(x)

    given Conversion[Address3A, Address3B] with
      override def apply(x: Address3A): Address3B = BeanBuilder.build[Address3B](x)("country"->"china") // additional dont support impicit conversion
      // override def apply(x: Address3A): Address3B = BeanBuilder.build[Address3B](x)(_.copy(country="china"))

    val person3A = Person3A("John", Address3A("123 Main St", "Anytown"))
    val person3B = Person3B("John", 20, Some(Address3B("123 Main St", "Anytown", "china")))
    assert(BeanBuilder.build[Person3B](person3A)("age"->20) == person3B)
    assert(BeanBuilder.build[Person3A](person3B) == person3A)
  }
  
  test("self reference"){

    assertDoesNotCompile("""
      case class User(name: String, age: Int, Parent: User)
      case class User2(name: String, age: Int, Parent: User2)
      val user = User("John", 20, User("Steven", 50, null))
    
      val user2 = BeanBuilder.build[User2](user)
      assert(user2 == User2("John", 20, User2("Steven", 50, null)))
    """)
  }

}
