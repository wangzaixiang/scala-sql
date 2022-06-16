package wangzx.scala_commons.sql

import java.sql.*
import scala.quoted.*

object ResultSetMapperMacro:

  inline def resultSetMapper[T] = ${resultSetMapperImpl[T]}

  def resultSetMapperImpl[T: Type](using Quotes): Expr[ResultSetMapper[T]] =
    import quotes.reflect.*

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
    def rsGetField[T:Type](rs: Expr[ResultSetWrapper], field: Symbol): Term =
      val name = field.name
      val defaultExpr: Expr[Option[Any]] = defaultParams.get(name) match
        case Some(deff) => '{ Some($deff) }
        case None => '{ None }

      val expr = field.tree.asInstanceOf[ValDef].tpt.tpe.asType match {
        case '[t] =>
          Expr.summon[JdbcValueAccessor[t]] match
            case Some(accessor) =>
              '{ new CaseField[t](${Expr(name)}, ${defaultExpr}.asInstanceOf[Option[t]])(using $accessor).apply($rs) }
            case None =>
              report.error(s"No JdbcValueAccessor found, owner:${TypeTree.of[T].show} field:$name type:${TypeTree.of[t].show}")
              '{ ??? }
      }
      expr.asTerm

    // '{ new T( field1, field2, ... ) }
    def buildBeanFromRs(rs: Expr[ResultSet]): Expr[T] =
      val tpeSym = TypeTree.of[T].symbol
      val _rsWrapper: Expr[ResultSetWrapper] = '{ new ResultSetWrapper($rs) }

      ValDef.let( Symbol.spliceOwner, _rsWrapper.asTerm) { _rsRef =>
        val terms: List[Term] = tpeSym.caseFields.map(field => rsGetField[T](_rsRef.asExpr.asInstanceOf[Expr[ResultSetWrapper]], field))
        val companion = tpeSym.companionModule
        val applyMethod = companion.memberMethod("apply").apply(0)
        ValDef.let(Symbol.spliceOwner, terms) { refs =>
          Apply(Select(Ref(companion), applyMethod), refs)
        }
      }.asExpr.asInstanceOf[Expr[T]]

    '{
    new ResultSetMapper[T]:
      def from(rs: ResultSet): T = ${buildBeanFromRs('{rs})}
    }