package wangzx.scala_commons.sql

import java.io.FileInputStream
import java.sql.{Connection, Driver, ResultSet}

import scala.util.Properties
//import java.util.Properties
import javax.sql.DataSource

import scala.language.experimental.macros


object Macros {

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
    assert( t.tpe.typeSymbol.asClass.isCaseClass, s"only support CaseClass, but ${t.tpe.typeSymbol.fullName} is not" )

    val companion = t.tpe.typeSymbol.asClass.companion

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

  def parseSQL(c: reflect.macros.blackbox.Context)(args: c.Tree*): c.Tree = {
    import c.universe._

    val q"""$a(scala.StringContext.apply(..$literals))""" = c.prefix.tree

    val it: c.Tree = c.enclosingClass
    val db: String = it.symbol.annotations.flatMap { x =>
      x.tree match {
        case q"""new $t($name)""" if t.symbol.asClass.fullName == classOf[db].getName =>
          val Literal(Constant(str: String)) = name
          Some(str)
        case _ => None
      }
    } match {
      case Seq(name) => name
      case _ => "default"
    }


    val x: List[Tree] = literals
    //println(s"x type = ${x.getClass}")
    // literals.children.foreach(println)

    assert( x.forall( arg => arg.isInstanceOf[Literal] ) )
    //val args2: c.Tree =

    val stmt = x.map { case Literal(Constant(value: String)) => value }.mkString("?")

    // to disable sql checking(eg, no local database exists, define the -Dscala_sql.check=false
    // or using env SCALA_SQL_CHECK=false
    val checkSqlFlag = {
      System.getProperty("scala_sql.check") match {
        case "false" | "FALSE" => false
        case null =>
          System.getenv("SCALA_SQL_CHECK") match {
            case "false" | "FALSE" => false
            case _ => true
          }
        case _ => true
      }
    }
    if(checkSqlFlag) {
      try {
        SqlChecker.checkSqlGrammar(db, stmt, args.length)
      }
      catch {
        case ex: Throwable =>
          //ex.printStackTrace()
          c.error(c.enclosingPosition, s"SQL grammar erorr ${ex.getMessage}")
      }
    }

    q"""wangzx.scala_commons.sql.SQLWithArgs($stmt, Seq(..$args))"""
  }

  object SqlChecker {

    // lazy val compilerDataSources = collection.mutable.Map[String, Connection]()
    lazy val properties = {
      val config = new java.util.Properties()
      val path = s"${System.getProperty("user.dir")}/scala-sql.properties"
      try {
        config.load(new FileInputStream(path))
      }
      catch {
        case ex: Throwable =>
          println(s"can't load $path")
          val path2 = s"${System.getProperty("user.home")}/.scala-sql.properties"
          try {
            config.load(new FileInputStream(path2))
          }
          catch {
            case ex: Throwable =>
              println(s"can't load $path2")
          }
      }

      config
    }
    def getConnection(db: String): Connection = {
        val url = properties.getProperty(s"${db}.url")
        val user = properties.getProperty(s"${db}.user")
        val password = properties.getProperty(s"${db}.password")
        val driver = properties.getProperty(s"${db}.driver")

        val driverObj = Class.forName(driver).newInstance().asInstanceOf[Driver]
        val props = new java.util.Properties()
        props.put("user", user)
        props.put("password", password)

        driverObj.connect(url, props)
    }

    def checkSqlGrammar(db: String, stmt: String, argc: Int): Unit = {
      val conn = getConnection(db)
      // conn.setReadOnly(true)
      try {
        conn.setAutoCommit(false)
        val ps = conn.prepareStatement(s"explain $stmt")
        for(i <- 1 to argc) ps.setNull(i, java.sql.Types.VARCHAR)
        ps.executeQuery()
      }
      finally {
        conn.rollback()
        conn.close()
      }
    }
  }

  def desugar(a: Any): String = macro desugarImpl

  def desugarImpl(c: scala.reflect.macros.whitebox.Context)(a: c.Tree) = {
    import c.universe._

    val s = show(a)
    q"""$s"""
  }

}
