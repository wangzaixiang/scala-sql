package wangzx.scala_commons.sql

import scala.language.experimental.macros

object BeanBuilder {

  /**
    * BeanBuilder.build is a utils to build a target case-class instance from other case-class instance.
    *
    * It follows the simple name-to-name field copy, if the default copy not works for you, you may
    * define custom copies in the adds arguament.
    *
    * @param sources
    * @param adds such as "name" -> name etc
    * @tparam T the taregt bean
    * @return
    */
  def build[T](sources: Any*)(adds: (String, Any)*) : T = macro buildImpl[T]


  def buildImpl[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context)(sources: c.Tree*)(adds: c.Tree*): c.Tree = {

    import c.{universe => u}
    import u._

    val t = implicitly[c.WeakTypeTag[T]]

    // dest fields
    val fields: List[String] = t.tpe.members.toList.filter(m => m.isMethod && m.asMethod.isCaseAccessor).map (_.name.toString)

    val sourceFields : Map[c.Tree, List[String]] = sources.map { (src: c.Tree) =>
      ( src,
        src.tpe.members.toList.filter(m => m.isMethod && m.asMethod.isCaseAccessor).map(_.name.toString)
      )
    }.toMap

    val additionByName: Map[String, c.Tree] = adds.toList.map { add=>
      val method = TermName("->")
      //println(s"add = ${u.showRaw(add)}")
      val (a: c.Tree,b: c.Tree) = add match {
        case q"($a, $b)" => (a, b)
        case q"scala.Predef.ArrowAssoc[..$x]($a).->[..$y]($b)" => (a, b)
        case q"scala.this.Predef.ArrowAssoc[..$x]($a).->[..$y]($b)" => (a, b)
        case _ =>
          println(s"can't match ${u.showRaw(add)}")
          throw new AssertionError()
      }
      val Literal(Constant(str: String)) = a
      (str, b)
    }.toMap

    val parameters: List[c.Tree] =
      fields.flatMap { field: String =>
        val termName = TermName(field)
        if(additionByName contains field) {
          val add = additionByName(field)
          Some(q"""$termName = $add""")
        }
        else {
          sourceFields.toList.filter(x => x._2 contains field) match {
            case List((tree, _)) =>
              Some(q"""$termName = $tree.$termName""")
            case Nil => // Not found
              None
            case x@_ => // 2+ matches
              val names = x.map(_._1.symbol.name).mkString(",")
              throw new AssertionError(s"$field has ambiguous, it occurs in ${names}, to avoid it, you can define in the adds args.")
          }

        }
      }

    q"""new $t( ..$parameters )"""
  }

}
