package wangzx.scala_commons.sql

import scala.quoted.*
import java.sql.Connection

object Macros {

  def createBatchImpl[T: Type](proc: Expr[T=>SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =
    '{ null }

  def createMysqlBatchImpl[T: Type](proc: Expr[T=>SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =
    '{ null }

  def buildImpl1[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[Seq[(String, Any)]])(using Quotes): Expr[T] =
    import quotes.reflect.*

    // println TypeTree for each source
    val sourceFields: Map[Term, Map[String, Symbol]] = sources.asTerm match
      case Inlined(_, Nil, Typed(Repeated(terms, tpt1), tpt2)) =>
        terms.filter(_.tpe.typeSymbol.flags.is(Flags.Case)).map { term =>
          val tpeSym = term.tpe.typeSymbol
          (term, tpeSym.caseFields.map( f => (f.name, f) ).toMap)
        }.toMap
      case _ =>
        println("*** unmatched")
        Map()
    println("sourceFields: " + sourceFields)

    val adds: List[(String, Expr[Any])] = additions.asTerm match
      case Inlined(_, Nil, Typed(Repeated(terms, _), _)) =>
        terms.flatMap { term =>
          // println("\tterm:" + term.show(using Printer.TreeStructure))
          term.asExpr match {
            case '{ ($a: String) -> ($b: Any) } =>
              Some( Expr.unapply(a).get -> b )
            case _ => // TODO support (String, Any)
              report.error("*** unmatched")
              None
          }
        }
      case _ =>
        report.error("*** unmatched")
        Nil
    println("adds: " + adds)

    val fields = TypeTree.of[T].tpe.typeSymbol.caseFields
    println("fields: " + fields)

    val terms: List[Term] = fields.map { f =>
      sourceFields.find(_._2.contains(f.name)).map { case (term, _) =>
        Select.unique(term, f.name)
      } match {
        case Some(term) => term
        case None =>
          report.error(s"*** unmatched for field ${f.name}")
          ???
      }
    }

    val block = ValDef.let(Symbol.spliceOwner, terms) { refs =>
      // using the let method to generate a block
      val companion = TypeTree.of[T].symbol.companionModule
      val applyMethod = companion.memberMethod("apply").apply(0)
      Apply( Select(Ref(companion), applyMethod), refs)
    }

    def dump[T](source: Expr[T]): Expr[T] =
      println(source.show)
      println(source.asTerm.show(using Printer.TreeStructure))
      source

    println("!!!! generate blcok")
    dump(block.asExpr)
    block.asExpr.asInstanceOf[Expr[T]]

  def buildImpl2[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[T=>T])(using Quotes): Expr[T] =
    '{ null.asInstanceOf[T] }

}
