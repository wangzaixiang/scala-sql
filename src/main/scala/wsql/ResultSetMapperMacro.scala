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

    // '{ new CaseField[f.type](f.name, f.default)(using JdbcValueMapper[f.type])($rs) }
    def rsGetField(rsColumns: Expr[Set[String]], rs: Expr[ResultSet], field: Symbol): Term =
      val name = field.name
      val columnName = columnMapper.columnName(name)

      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match {
        case '[t] =>
          Expr.summon[JdbcValueAccessor[t]] match
            case Some(accessor) =>
              val defaultExpr: Expr[Option[t]] = defaultParams.get(name) match
                case Some(deff) => '{ Some(${deff.asInstanceOf[Expr[t]]}) }
                case None => '{ None }
              '{ caseFieldGet[t]($rsColumns, ${Expr(columnName)}, ${Expr(columnName.toUpperCase)},${defaultExpr}, $rs)(using $accessor) }
            case None =>
              report.error(s"No JdbcValueAccessor found, owner:${TypeTree.of[T].show} field:$name type:${TypeTree.of[t].show}")
              '{ ??? }
      }
      expr.asTerm

    // '{ new T( field1, field2, ... ) }
    def buildBeanFromRs(rs: Expr[ResultSet]): Expr[T] =
      val tpeSym = TypeTree.of[T].symbol
      val _rsColumns: Expr[Set[String]] = '{ getResultSetFieldNames($rs) }
//      val _rsWrapper: Expr[ResultSetWrapper] = '{ new ResultSetWrapper($rs) }

      ValDef.let( Symbol.spliceOwner, _rsColumns.asTerm) { _rsColumnsRef =>
        val terms: List[Term] = tpeSym.caseFields.map(field => rsGetField(_rsColumnsRef.asExpr.asInstanceOf[Expr[Set[String]]], rs, field))
        val constructor = TypeTree.of[T].symbol.primaryConstructor
        ValDef.let(Symbol.spliceOwner, terms) { refs =>
          Apply(Select(New(TypeTree.of[T]), constructor), refs)
        }
      }.asExpr.asInstanceOf[Expr[T]]

    val expr = '{
      new ResultSetMapper[T]:
        def from(rs: ResultSet): T = ${buildBeanFromRs('{rs})}
    }

//    println("expr: " + expr.show)

    expr
