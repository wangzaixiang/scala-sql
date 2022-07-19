package wsql.macros

import scala.quoted.*
import java.sql.Connection
import scala.deriving.Mirror

object BuilderMacros {

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

  // tryBuild[ String, Int ]
  // TODO process self-reference
  private def tryBuildExpr[f: Type, t:Type](src: Expr[f])(using Quotes): Option[Expr[t]] =
    import quotes.reflect.*

    def isOption[a:Type]: Boolean =
      TypeRepr.of[a] <:< TypeRepr.of[Option[?]]

    def directConvert: Option[Expr[t]] =
      if TypeRepr.of[f] <:< TypeRepr.of[t] then Some(src.asInstanceOf[Expr[t]])
      else None

    def implicitConversion: Option[Expr[t]] = Expr.summon[Conversion[f,t]] match
      case Some(conv) => Some('{ $conv.apply($src) })
      case None => None

    // X[a] => X[b]
    def mapElement: Option[Expr[t]] =
      (TypeRepr.of[f].widen, TypeRepr.of[t].widen) match
        case (AppliedType(f_base, f_args), AppliedType(t_base, t_args)) if f_base == t_base && f_args.size == 1 && t_args.size == 1 && f_args(0) != t_args(0) =>

          val typeXB: Type[?] = f_base.appliedTo(t_args).asType
          (typeXB, f_args(0).asType, t_args(0).asType) match
            case ('[xb], '[a], '[b]) =>
              val element_conversion = tryBuildFunc[a,b]
              element_conversion match
                case Some(conv) =>
                  val map = Select.unique(src.asTerm, "map")
                  val typeApply = TypeApply( map, List( TypeTree.of[b] ) )
                    Some( Apply(typeApply, List(conv.asTerm)).asExpr.asInstanceOf[Expr[t]] )
                case _ => None

        case _ => None

    // X[a] => Y[b]
    def mapContainerAndElement: Option[Expr[t]] =
      (TypeRepr.of[f].widen, TypeRepr.of[t].widen) match
        case (AppliedType(f_base, f_args), AppliedType(t_base, t_args)) if f_base != t_base && f_args.size == 1 && t_args.size == 1 && f_args(0) != t_args(0) =>

          val typeYA: Type[?] = t_base.appliedTo(f_args).asType
          (typeYA, f_args(0).asType, t_args(0).asType) match
            case ('[ya], '[a], '[b]) =>
              val base_conversion = Expr.summon[ Conversion[f,ya] ]

              val element_conversion = tryBuildFunc[a,b]

              (base_conversion, element_conversion) match
                case (Some(conv1), Some(conv2)) =>
                  val x = '{ $conv1($src) }
                  val map = Select.unique(x.asTerm, "map")
                  val typeApply = TypeApply( map, List( TypeTree.of[b] ) )
                  Some( Apply(typeApply, List(conv2.asTerm)).asExpr.asInstanceOf[Expr[t]] )
                case _ => None

        case _ => None

    // A => Option[B]
    def toOption: Option[Expr[t]] =
      val isPrimitive = TypeRepr.of[f] <:< TypeRepr.of[AnyVal]
      if !isOption[f] && isOption[t] then
        TypeRepr.of[t].widen match
          case AppliedType(t_base, t_args) =>
            t_args(0).asType match
              case '[t2] =>
                tryBuildExpr[f, t2](src).map ( conv =>
                  if(isPrimitive)
                    '{ Option($conv).asInstanceOf[t] }
                  else
                    '{ (if($src == null) None else Option($conv)).asInstanceOf[t] }
                )
      else None

    // Option[A] => B
    def unapplyOption: Option[Expr[t]] =
      if( isOption[f] && !isOption[t]) then
        TypeRepr.of[f].widen match
          case AppliedType(f_base, f_args) =>
            f_args(0).asType match
              case '[f2] =>
                tryBuildFunc[f2,t].map( conv => // TODO primitive type
                    '{ $src.asInstanceOf[Option[f2]].map($conv).getOrElse(null.asInstanceOf[t]) }
                )
      else None

    def caseClassConvert: Option[Expr[t]] =
      if( TypeRepr.of[f].widen.typeSymbol.flags.is(Flags.Case) && TypeRepr.of[t].widen.typeSymbol.flags.is(Flags.Case) ) then
        val args = '{ Seq($src) }
        Some( buildCaseClassImpl[t](args.asInstanceOf[Expr[Seq[AnyRef]]])('{Seq()} ) )
      else None

    directConvert
      .orElse(implicitConversion)
      .orElse(toOption)
      .orElse(unapplyOption)
      .orElse(mapElement)
      .orElse(mapContainerAndElement)
      .orElse(caseClassConvert)

  private def tryBuildFunc[f: Type, t: Type](using Quotes): Option[Expr[Function1[f,t]]] =
    try
      Some( '{ (src: f) => ${ tryBuildExpr[f,t]('{src}).get } } )
    catch
      case ex: NoSuchElementException => None

  def extractSeq(seqs: Expr[Seq[Any]])(using quotes: Quotes): List[quotes.reflect.Term] =
    import quotes.reflect.*
    seqs.asTerm match
      case Inlined(_, Nil, Typed(Repeated(terms, _), _)) => terms
      case Inlined(_, Nil, Apply( TypeApply( Select(seq, "apply"), _), List(Typed(Repeated(terms, _), _)))) => terms
      case _ => Nil

  // TODO self-type reference
  def buildCaseClassImpl[T:Type](sources: Expr[Seq[AnyRef]])(additions: Expr[Seq[(String, Any)]])(using Quotes): Expr[T] =
    import quotes.reflect.*

    assert( TypeTree.of[T].symbol.flags.is(Flags.Case) )

    // src -> fieldName -> fieldSymbol
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
              report.error("*** unmatched expr " + term.show)
              None
          }
        }.toMap

    val fields = TypeTree.of[T].tpe.typeSymbol.caseFields

    val defaultParams: Map[String, Expr[Any]] = defaultParamsForCaseClass[T]

    def dump[T](source: Expr[T]): Expr[T] =
      println("dump:" + source.show)
      println("dump:" + source.asTerm.show(using Printer.TreeStructure))
      source

    val fieldExprs: List[Term] = fields.map { field =>
      val name = field.name

      def defaultValue: Option[Expr[Any]] = defaultParams.get(name) match {
        case Some(expr) => Some(expr)
        case None => // test field's type is Option[?]
          val isOption = field.tree.asInstanceOf[ValDef].tpt.tpe.widen <:< TypeRepr.of[Option[?]]
          if isOption then Some('{ None })
          else None
      }

      def fromSourceFields: Option[Expr[Any]] =
        sourceFields.toList.filter{ case (term, fields) => fields.contains(name) }.map {
          case (term, fields) =>
            Select.unique(term, name).asExpr.asInstanceOf[Expr[Any]]
        } match {
          case Nil => None
          case x :: Nil => // TODO exist field but type not matched, prompt error
            (x.asTerm.tpe.asType, field.tree.asInstanceOf[ValDef].tpt.tpe.asType) match
              case ('[f], '[t]) =>
                tryBuildExpr[f, t](x.asInstanceOf[Expr[f]])
          case _ =>
            report.error(s"field $name exists in multiple sources")  // TODO more information
            None
        }

      val expr = additionParams.get(name)
        .orElse(fromSourceFields)
        .orElse(defaultValue)

      expr match
        case Some(expr) => expr.asTerm
        case None =>
          report.error(s"no value for field $name")
          ???
    }

    val constructor = TypeTree.of[T].tpe.typeSymbol.primaryConstructor

    val block = ValDef.let( Symbol.spliceOwner, fieldExprs  ) { refs =>
      Apply( Select( New(TypeTree.of[T]), constructor), refs)
    }.asExpr.asInstanceOf[Expr[T]]

//    dump(block)

    block

}
