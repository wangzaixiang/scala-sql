package wangzx.scala_commons.sql

import java.sql.ResultSet

import scala.language.experimental.macros
import scala.reflect.macros.Context


object Macros {

//  case class User(name: String, age: Int, classRoom: Int = 1) {
//    def this(b: Boolean) = this("wangzx", 40)
//  }
//
//
//
//  /*
//      override def from(rs: ResultSet): User = {
//      val NAME = Field[String]("name")
//      val AGE = Field[Int]("age")
//      val CLASSROOM = Field[Int]("classRoom", Some("apply$default$3"))
//
//      User(NAME(rs), AGE(rs), CLASSROOM(rs))
//    }
//   */
//
//  case class Field[T](name: String) {
//    def apply(rs: ResultSet): T = ???
//  }
//
//  def fromRS(rs: ResultSet): User = macro fromRSMacro[User]
//
//  def fromRSMacro[A](c: Context)(rs: c.Tree): c.Tree = {
//    import c.universe._
//
//    val tree =
//    q"""
//        import wangzx.test.Macros._
//
//        val NAME = Field[String]("name")
//        val AGE = Field[Int]("age")
//        val CLASSROOM = Field[Int]("classRoom", Some("apply$$default$$3") )
//
//        User( NAME($rs), AGE($rs), CLASSROOM($rs) )
//     """
//    println(tree)
//
//    tree
//  }

  //def implictUserMapper: ResultSetMapper[User] = macro implictMapper[User]
  /*
  // Case class Style
  implicit object UserMapper extends CaseClassResultSetMapper[User](User) {

    override def from(rs: ResultSet): User = {
      val NAME = Field[String]("name")
      val AGE = Field[Int]("age")
      val CLASSROOM = Field[Int]("classRoom", Some("apply$default$3"))

      User(NAME(rs), AGE(rs), CLASSROOM(rs))
    }


  }
  }
   */
  def generateCaseClassResultSetMapper[T: c.WeakTypeTag](c: scala.reflect.macros.whitebox.Context): c.Tree = {
    import c.universe._

    val t: c.WeakTypeTag[T] = implicitly[c.WeakTypeTag[T]]
    val companion = t.tpe.typeSymbol.asClass.companion

//    println(s"companion = ${companion}")
//    companion.asModule.typeSignature.decls.foreach(println);

//    println(s" default = ${ companion.asModule.typeSignature.member( TermName("$lessinit$greater$default$3") ) } ")

    val constructor: c.universe.MethodSymbol = t.tpe.typeSymbol.asClass.primaryConstructor.asMethod

    var index = 0
    val args: List[(c.Tree, c.Tree)] = constructor.paramLists(0).map { (p: c.universe.Symbol) =>
      val term: c.universe.TermSymbol = p.asTerm
      index += 1

      // search "apply$default$X"
      val name = term.name.toString
      val newTerm = TermName(name.toString)

      val tree = if(term.isParamWithDefault) {
        val defMethod: c.universe.Symbol = t.tpe.typeSymbol.asClass.companion.asModule.typeSignature.member(TermName("$lessinit$greater$default$" + index))
        q"""val $newTerm = Field[${term.typeSignature}]($name, Some($companion.$defMethod) )"""
      }
      else
        q"""val $newTerm = Field[${term.typeSignature}]($name) """

      (q"""${newTerm}(rs)""", tree)
    }

//    val defaultMethod: c.universe.Symbol = t.tpe.typeSymbol.asClass.companion.asModule.typeSignature.member(TermName("apply$default$3"))
//
//    val tree = q"""
//       import wangzx.scala_commons.sql._
//       import java.sql.ResultSet
//
//       new CaseClassResultSetMapper[$t]($companion) {
//        val name = Field[String]("name")
//        val age = Field[Int]("age")
//        val classRoom = Field[Int]("classRoom", Some($companion.$defaultMethod))
//
//        override def from(rs: ResultSet): $t = {
//          new $t( name(rs), age(rs), classRoom(rs) )
//        }
//       }
//     """

    val tree =
      q"""
         import wangzx.scala_commons.sql._
         import java.sql.ResultSet

         new CaseClassResultSetMapper[$t] {
          ..${args.map(_._2)}

          override def from(arg: ResultSet): $t = {
            val rs = new ResultSetEx(arg)
            new $t( ..${args.map(_._1) } )
          }
         }
       """

//    println(tree);
    tree
  }

}
