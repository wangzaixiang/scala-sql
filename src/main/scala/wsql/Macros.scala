package wsql

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

  // Box[f] => Box[t] = src.map( summon[f=>t] )
  private def mapElement[f:Type, t:Type](using quotes:Quotes)(src: Expr[f]): Option[Expr[t]] = {
    import quotes.reflect.*
    val fw = TypeTree.of[f].tpe.widen
    val tw = TypeTree.of[t].tpe.widen

    (fw, tw) match
      case (AppliedType(fbase, fargs), AppliedType(tbase, targs)) if fbase == tbase && fargs.size == targs.size && fargs.size == 1 =>
        val farg:TypeRepr = fargs(0)
        val targ:TypeRepr = targs(0)

        (farg.asType, targ.asType) match
          case ('[fa], '[ta]) => // TODO check expr having map method
            Expr.summon[Conversion[fa, ta]] match
              case Some(conv) =>
                 val x = Apply(TypeApply(Select.unique(src.asTerm, "map"), List(TypeTree.of[ta])), List(conv.asTerm))
                 Some (x.asExpr.asInstanceOf[Expr[t]])
              case None => None
      case _ => None
  }

//  private def mapBox[f:Type, t:Type](using quotes:Quotes)(src: Expr[f]): Option[Expr[t]] =
//    import quotes.reflect.*
//
//    (TypeRepr.of[f].widen, TypeRepr.of[t].widen) match {
//      case (AppliedType(fbase, fargs), AppliedType(tbase, targs)
//        if fargs.size == targs.size && fargs.size == 1 && fargs(0) == targs(0) =>
//
//
//    }

//  private def dumpTypeRepr(using quotes: Quotes)(tpe: quotes.reflect.TypeRepr) = {
//    import quotes.reflect.*
//    println(s"show = ${tpe.show(using Printer.TypeReprStructure)}")
//    println("widen " + tpe.widen)
//    println("dealias = " + tpe.dealias)
//    println("typeSymbol = " + tpe.typeSymbol)
//    println("termSymbol = " + tpe.termSymbol)
//    println("isSingleton = " + tpe.isSingleton)
//    println("baseClasses = " + tpe.baseClasses)
//
//    tpe.typeSymbol.companionModule.methodMembers.foreach(dumpSymbol)
//    println();
//  }
//
//  private def dumpSymbol(using quotes: Quotes)(sym: quotes.reflect.Symbol) = {
//    if(sym.name == "apply") {
//      println("sym = " + sym)
//      println("params = " + sym.paramSymss)
//    }
//  }

  private def tryBuildImpl[f: Type, t: Type](using quotes:Quotes)(src: Expr[f]): Option[Expr[t]] =
    import quotes.reflect._

    if TypeRepr.of[f].widen.typeSymbol.flags.is(Flags.Case) && TypeRepr.of[t].widen.typeSymbol.flags.is(Flags.Case)
    then
      val x = '{ Seq($src) }
      Some( buildImpl1[t](x.asInstanceOf[Expr[Seq[AnyRef]]])('{Seq()} ) )
    else None


  private def convert(using quotes: Quotes)(src: Expr[Any], destTpe: quotes.reflect.TypeRepr): Expr[Any] =
    import quotes.reflect.*
    (src.asTerm.tpe.asType, destTpe.asType) match
      case ('[f], '[t]) =>
        val f_tpe = TypeTree.of[f].tpe
        val t_tpe = TypeTree.of[t].tpe

        def directConvert: Option[Expr[Any]] =
          if f_tpe <:< t_tpe then Some(src)
          else None

        def implicitConversion: Option[Expr[Any]] = Expr.summon[Conversion[f,t]] match
          case Some(conv) => Some( '{ $conv( ${src.asInstanceOf[Expr[f]]} ) } )
          case None => None

        // X[a] => Y[a]
        def boxConvert: Option[Expr[Any]] = None

        // X[a] => Y[b]
        def mapBoxAndElement: Option[Expr[Any]] = None

        // src.copyTo[t]
        def implicitCopyTo: Option[Expr[Any]] =
          try
            val term = Apply(TypeApply(Select.unique(src.asTerm, "copyTo"), List(TypeTree.of[t])), List())
            Some(term.asExpr.asInstanceOf[Expr[Any]])
          catch
            case ex: Throwable => None

        // T.apply(src)
        // TODO check the T has a matched apply method
        // Option.apply(src)
        // or Option.apply[f](src): t
        def simpleApply: Option[Expr[Any]] =
          // check t.apply
//          val tpe = TypeRepr.of[Option[Int]]
//          dumpTypeRepr(tpe)

          None
//          try
//            val term = Apply(TypeApply(Select.unique( Ref(TypeTree.of[t].symbol.companionModule), "apply"), List(TypeTree.of[f])), List(src.asTerm))
//            println("term  = " + term.show)
//            Some(term.asExpr.asInstanceOf[Expr[Any]])
//          catch
//            case ex: Throwable => None

        // F.unapply(src).get
        def simpleUnapply: Option[Expr[Any]] = None

        def failed: Expr[Any] =
          report.error(s"*** unsupported conversion ${f_tpe.show} -> ${t_tpe.show}")
          '{ ??? }

        directConvert
          .orElse(implicitConversion)
          .orElse( mapElement[f,t](src.asInstanceOf[Expr[f]]) )
          // todo X[a] => X[b]  if exists Conversion[a,b]
          // todo X[a] => Y[a]  if exists Converion[X[a], Y[a]]
          // todo X[a] => Y[b]  if exists Conversion[a, b] && Conversion[X[a], Y[a]]
          .orElse( simpleApply )
          .orElse( tryBuildImpl[f,t](src.asInstanceOf[Expr[f]]) )
          .getOrElse(failed)


  def extractSeq(seqs: Expr[Seq[Any]])(using quotes: Quotes): List[quotes.reflect.Term] =
    import quotes.reflect.*
    seqs.asTerm match
      case Inlined(_, Nil, Typed(Repeated(terms, _), _)) => terms
      case Inlined(_, Nil, Apply( TypeApply( Select(seq, "apply"), _), List(Typed(Repeated(terms, _), _)))) => terms
      case _ => Nil

  def buildImpl1[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[Seq[(String, Any)]])(using Quotes): Expr[T] =
    import quotes.reflect.*

    assert( TypeTree.of[T].symbol.flags.is(Flags.Case) )

    // Expr[Seq[AnyRef]] => Seq[Expr[AnyRef]]
    val sourceFields: Map[Term, Map[String, Symbol]] = extractSeq(sources).filter(_.tpe.typeSymbol.flags.is(Flags.Case))
      .map { term =>
        val tpeSym = term.tpe.typeSymbol
        (term, tpeSym.caseFields.map( f => (f.name, f) ).toMap)
      }.toMap

    val additionParams: Map[String, Expr[Any]] = extractSeq(additions).flatMap { term =>
          term.asExpr match {
            case '{ ($a: String) -> ($b: Any) } =>
              Some( Expr.unapply(a).get -> b )
            case _ => // TODO support (String, Any)
              report.error("*** unmatched 144")
              None
          }
        }.toMap

    val fields = TypeTree.of[T].tpe.typeSymbol.caseFields

    val defaultParams: Map[String, Expr[Any]] = defaultParamsForCaseClass[T]

    def dump[T](source: Expr[T]): Expr[T] =
      println("dump:" + source.show)
      println("dump:" + source.asTerm.show(using Printer.TreeStructure))
      source

    val fieldMaps: List[(Symbol, Expr[Any])] = fields.map { field =>
      val name = field.name

      def defaultValue: Option[Expr[Any]] = defaultParams.get(name) match {
        case Some(expr) => Some(expr)
        case None => // test field's type is Option[?]
          val isOption = field.tree.asInstanceOf[ValDef].tpt.tpe.widen.typeSymbol == TypeTree.of[Option[String]].tpe.widen.typeSymbol  // TODO
          if isOption then Some('{ None })
          else None
      }

      def fromSourceFields: Option[Expr[Any]] =
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

      val expr = additionParams.get(name)
        .orElse(fromSourceFields)
        .orElse(defaultValue)

      expr match
        case Some(expr) => (field, convert(expr, field.tree.asInstanceOf[ValDef].tpt.tpe))
        case None =>
          report.error(s"field $name not found")
          ???
    }

    val block = ValDef.let(Symbol.spliceOwner, fieldMaps.map(_._2.asTerm)) { refs =>
      val companion = TypeTree.of[T].symbol.companionModule
      val applyMethod = companion.memberMethod("apply").apply(0)
      Apply( Select(Ref(companion), applyMethod), refs)
    }

    // dump('{ val x = List("123"); x.map(null: Conversion[String,Int])})

//    println("!!!! generate block now..")
//    dump(block.asExpr)
//    println("!!!")
    block.asExpr.asInstanceOf[Expr[T]]

  def buildImpl2[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[T=>T])(using Quotes): Expr[T] =
    '{ null.asInstanceOf[T] }

}
