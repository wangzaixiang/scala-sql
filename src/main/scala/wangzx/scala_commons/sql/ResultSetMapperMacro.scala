package wangzx.scala_commons.sql

import java.sql.*
import scala.quoted.*

object ResultSetMapperMacro:

  inline def resultSetMapper[T] = ${resultSetMapperImpl[T]}

  def resultSetMapperImpl[T: Type](using Quotes): Expr[ResultSetMapper[T]] =
    import quotes.reflect.*

    def exprForField[T: Type](field: Symbol): Expr[?] =
      val typetree = TypeTree.of[T]
      val nullObj = Typed(Literal(NullConstant()), typetree)
      val fieldTypeTree = field.tree.asInstanceOf[ValDef].tpt
      Typed( Select.unique (nullObj, field.name), fieldTypeTree).asExpr

    def defaultParams(): Map[String, Expr[Any]] =
      val sym = TypeTree.of[T].symbol
      val comp = sym.companionClass
      val module = Ref(sym.companionModule)

      val names = for p <- sym.caseFields if p.flags.is(Flags.HasDefault) yield p.name
      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Expr[?]] = for case deff @ DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default$")
        yield module.select(deff.symbol).asExpr

      names.zip(idents).toMap

    val defaults = defaultParams()

    def rsGetField[T:Type](rs: Expr[ResultSetWrapper], field: Symbol): (Symbol, ValDef) =
      val name = field.name
      val nameExpr = Expr(name)
      val expr = exprForField[T](field)
      val defaultExpr: Expr[Option[Any]] = defaults.get(name) match
        case Some(deff) => '{ Some($deff) }
        case None => '{ None }

      val rsGet = expr match {
        case '{ $x: t } =>
          val typet = TypeTree.of[t]
          Expr.summon[JdbcValueAccessor[t]] match
            case Some(accessor) =>
              '{ new CaseField[t]($nameExpr, ${defaultExpr}.asInstanceOf[Option[t]])(using $accessor).apply($rs) }
            case None =>
              report.error(s"No JdbcValueAccessor found, owner:${TypeTree.of[T].show} field:$name type:${typet.show}")
              '{ ??? }
      }

      val variable = Symbol.newVal(Symbol.spliceOwner, name,
        field.tree.asInstanceOf[ValDef].tpt.tpe, Flags.EmptyFlags, Symbol.noSymbol)
      val valdef = ValDef(variable, Some(rsGet.asTerm))
      (variable, valdef)


    def buildBeanFromRs(rs: Expr[ResultSet]) = {

      val tpeSym = TypeTree.of[T].symbol
      val children = tpeSym.caseFields

      val _rsSym = Symbol.newVal(Symbol.spliceOwner, "_rs", TypeTree.of[ResultSetWrapper].tpe, Flags.EmptyFlags, Symbol.noSymbol)
      val _rsExpr = '{ new ResultSetWrapper($rs) }
      val _rsVal = ValDef(_rsSym, Some(_rsExpr.asTerm))
      val _rsRef = Ref(_rsSym).asExpr.asInstanceOf[Expr[ResultSetWrapper]]

      val varsAndDefs: List[(Symbol, ValDef)] = children.map(rsGetField[T](_rsRef, _))

      val companion = tpeSym.companionModule
      val applyMethod = companion.memberMethod("apply").apply(0)

      val args: List[Ref] = varsAndDefs.map(x => Ref(x._1))
      val stmts: List[ValDef] = varsAndDefs.map(x => x._2)

      val apply = Apply(
        Select( Ref(companion), applyMethod),
        args )

      val block = Block(_rsVal :: stmts, apply)
      block.asExpr.asInstanceOf[Expr[T]]
    }

    '{
    new ResultSetMapper[T]:
      def from(rs: ResultSet): T = ${buildBeanFromRs('{rs})}
    }

