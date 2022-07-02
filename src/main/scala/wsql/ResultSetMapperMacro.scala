package wsql

import java.sql.*
import scala.quoted.*

// TODO refact code from experience of wjson
object ResultSetMapperMacro:

  inline def resultSetMapper[T] = ${resultSetMapperImpl[T]}

  def resultSetMapperImpl[T: Type](using Quotes): Expr[ResultSetMapper[T]] =
    import quotes.reflect.*

    // extract @ColumnMapper(classOf[T]) if exists
    val columnMapper: CaseClassColumnMapper =
      val classSymbol = TypeTree.of[T].symbol
      val columnMapperSymbol = TypeTree.of[UseColumnMapper].symbol
      classSymbol.getAnnotation(columnMapperSymbol) match
        case Some(term) =>
          term.asExpr match   // @ColumnMapper(classOf[T])
            case '{ UseColumnMapper(${value}) } =>
              value match
                case '{ $x: t } =>
                  // println("x: " + x.show + " t: " + TypeTree.of[t].show)
                  TypeRepr.of[t].widen.asInstanceOf[AppliedType] match
                    case AppliedType(base, List(clazz)) =>
                      Class.forName(clazz.widen.show).newInstance().asInstanceOf[CaseClassColumnMapper]
        case None => IdentityMapping()

    val defaultParams: Map[String, Expr[Any]] =
      val sym = TypeTree.of[T].symbol
      val comp = sym.companionClass
      val module = Ref(sym.companionModule)

      val names = for p <- sym.caseFields if p.flags.is(Flags.HasDefault) yield p.name
      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Expr[?]] = for case deff @ DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default$")
        yield module.select(deff.symbol).asExpr

      names.zip(idents).toMap

    def isPrimitive(tpe: TypeRepr): Boolean =
      val anyVal = TypeRepr.of[AnyVal]
      tpe <:< anyVal

    // '{ new CaseField[f.type](f.name, f.default)(using JdbcValueMapper[f.type])($rs) }
    def rsGetField(rs: Expr[ResultSet], field: Symbol): Term =
      val name = field.name
      val columnName = columnMapper.columnName(name)

      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match {
        case '[t] =>
          val primtive = isPrimitive(TypeRepr.of[t])
          val isOption = TypeRepr.of[t].widen <:< TypeRepr.of[Option[Any]]

          Expr.summon[JdbcValueAccessor[t]] match
            case Some(accessor) =>
              val (defaultExpr:Expr[Option[t]], none) = defaultParams.get(name) match
                case Some(deff) => ('{ Some(${deff.asInstanceOf[Expr[t]]}) }, false)
                case None => if isOption then ('{ Some(None.asInstanceOf[t]) }, false)  else ('{ None }, true)

              // TODO optimize option for inline
              if none == true then
                '{ withoutDefault[t](${Expr(columnName)}, $rs)(using $accessor) }
              else
                '{ withDefault[t](${Expr(columnName)}, ${Expr(primtive)}, ${defaultExpr}.asInstanceOf[Some[t]], $rs)(using $accessor) }
            case None =>
              report.error(s"No JdbcValueAccessor found, owner:${TypeTree.of[T].show} field:$name type:${TypeTree.of[t].show}")
              '{ ??? }
      }
      expr.asTerm

    // '{ new T( field1, field2, ... ) }
    def buildBeanFromRs(rs: Expr[ResultSet]): Expr[T] =
      val tpeSym = TypeTree.of[T].symbol

      val terms: List[Term] = tpeSym.caseFields.map(field => rsGetField(rs, field))
      val constructor = TypeTree.of[T].symbol.primaryConstructor
      ValDef.let(Symbol.spliceOwner, terms) { refs =>
        Apply(Select(New(TypeTree.of[T]), constructor), refs)
      }.asExpr.asInstanceOf[Expr[T]]

    val expr = '{
      new ResultSetMapper[T]:
        def from(rs: ResultSet): T = ${buildBeanFromRs('{rs})}
    }

//    println("expr: " + expr.show)

    expr
