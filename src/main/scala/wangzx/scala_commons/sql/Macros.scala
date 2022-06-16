package wangzx.scala_commons.sql

import scala.quoted.*
import java.sql.Connection

object Macros {

  def defaultParamsForCaseClass[T: Type](using Quotes): Map[String, Expr[Any]] =
    import quotes.reflect._
    val sym = TypeTree.of[T].symbol
    val comp = sym.companionClass
    val module = Ref(sym.companionModule)

    val names = for p <- sym.caseFields if p.flags.is(Flags.HasDefault) yield p.name
    val body = comp.tree.asInstanceOf[ClassDef].body
    val idents: List[Expr[?]] = for case deff @ DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default$")
      yield module.select(deff.symbol).asExpr

    names.zip(idents).toMap

  def createBatchImpl[T: Type](proc: Expr[T=>SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =
    '{ null }

  def createMysqlBatchImpl[T: Type](proc: Expr[T=>SQLWithArgs], conn: Expr[Connection])(using Quotes): Expr[Batch[T]] =
    '{ null }

  def buildImpl1[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[Seq[(String, Any)]])(using Quotes): Expr[T] =
    import quotes.reflect.*

    val sourceFields: Map[Term, Map[String, Symbol]] = sources.asTerm match
      case Inlined(_, Nil, Typed(Repeated(terms, tpt1), tpt2)) =>
        terms.filter(_.tpe.typeSymbol.flags.is(Flags.Case)).map { term =>
          val tpeSym = term.tpe.typeSymbol
          (term, tpeSym.caseFields.map( f => (f.name, f) ).toMap)
        }.toMap
      case _ =>
        println("*** unmatched")
        Map()

    val adds: Map[String, Expr[Any]] = additions.asTerm match
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
        }.toMap
      case _ =>
        report.error("*** unmatched")
        Map()

    val fields = TypeTree.of[T].tpe.typeSymbol.caseFields

    val defaultParams: Map[String, Expr[Any]] = defaultParamsForCaseClass[T]

    def dump[T](source: Expr[T]): Expr[T] =
      println("dump:" + source.show)
      println("dump:" + source.asTerm.show(using Printer.TreeStructure))
      source

    // dump('{ val x = List("123"); x.map(null: Conversion[String,Int])})
    // TODO support converters
    def convert(expr: Expr[Any], field: Symbol): Expr[Any] =
      (expr.asTerm.tpe.asType, field.tree.asInstanceOf[ValDef].tpt.tpe.asType) match
        case ('[f], '[t]) =>
          val f_tpe = TypeTree.of[f].tpe
          val t_tpe = TypeTree.of[t].tpe

          def directConvert: Option[Expr[Any]] =
            if f_tpe <:< t_tpe then Some(expr)
            else None

          def implicitConversion: Option[Expr[Any]] = Expr.summon[Conversion[f,t]] match
            case Some(conv) => Some( '{ $conv( ${expr.asInstanceOf[Expr[f]]} ) } )
            case None => None

          // Box[F] => Box[T]
          def boxMap: Option[Expr[Any]] =
            val f1 = TypeTree.of[f].tpe.widen
            val t1 = TypeTree.of[t].tpe.widen

            // TypeRepr -> TypeTree
            (f1, t1) match {
              case (AppliedType(fbase, fargs), AppliedType(tbase, targs)) if fbase == tbase && fargs.size == targs.size && fargs.size == 1 =>
                val farg = fargs(0)
                val targ = targs(0)

                (farg.asType, targ.asType) match {
                  case ('[fa], '[ta]) =>
                    // TODO check expr having map method
                    Expr.summon[Conversion[fa, ta]] match
                      case Some(conv) =>
                        val x = Apply( TypeApply( Select.unique( expr.asTerm, "map"), List(TypeTree.of[ta])), List(conv.asTerm))
                        Some( x.asExpr )
                      case None => None
                }
            }

          def failed: Expr[Any] =
            report.error(s"*** unsupported conversion ${f_tpe.show} -> ${t_tpe.show}")
            '{ ??? }

          directConvert
            .orElse(implicitConversion)
            .orElse(boxMap)
            .getOrElse(failed)

    val fieldMaps: List[(Symbol, Expr[Any])] = fields.map { field =>
      val name = field.name

      def defaults = defaultParams.get(name) match {
        case Some(expr) => Some(expr)
        case None => // test field's type is Option[?]
          val tpe = field.tree.asInstanceOf[ValDef].tpt.tpe
          val isOption = false // TODO
          if isOption then Some('{ None })
          else None
      }

      def fromSources: Option[Expr[Any]] =
        sourceFields.toList.filter { case (term, fields) => fields.contains(name) }.map {
          case (term, fields) =>
            Select.unique(term, name).asExpr.asInstanceOf[Expr[Any]]
        } match {
          case Nil => None
          case x :: Nil => Some(x)
          case _ =>
            report.error(s"field $name exists in multiple sources")  // TODO more information
            None
        }

      val expr = adds.get(name)
        .orElse(fromSources)
        .orElse(defaults)

      expr match
        case Some(expr) => (field, convert(expr, field))
        case None =>
          report.error(s"field $name not found")
          ???
    }

    val block = ValDef.let(Symbol.spliceOwner, fieldMaps.map(_._2.asTerm)) { refs =>
      val companion = TypeTree.of[T].symbol.companionModule
      val applyMethod = companion.memberMethod("apply").apply(0)
      Apply( Select(Ref(companion), applyMethod), refs)
    }



//    println("!!!! generate block now..")
//    dump(block.asExpr)
//    println("!!!")
    block.asExpr.asInstanceOf[Expr[T]]

  def buildImpl2[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[T=>T])(using Quotes): Expr[T] =
    '{ null.asInstanceOf[T] }

}
