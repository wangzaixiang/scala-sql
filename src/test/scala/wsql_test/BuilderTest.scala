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

  }

  test("optional fields and default fields") {

    import BeanBuilder.CollectionConverts.given

    case class Person2A(name: String, address: Seq[Address2A])
    case class Address2A(street: String, city: String)

    case class Person2B(name: String, age: Int = 0, address: List[Address2B])
    case class Address2B(street: String, city: String = "**")

    val person2A = Person2A("John", Seq( Address2A("123 Main St", "Anytown") ))
    val person2B = Person2B("John", 0, List( Address2B("123 Main St", "Anytown") ))

    println("12345")
    assert( BeanBuilder.build[Person2B](person2A) == person2B )
    assert( BeanBuilder.build[Person2B](person2A) == person2B )

  }

}
