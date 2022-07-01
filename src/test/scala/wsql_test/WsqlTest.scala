package wsql_test

import org.scalatest.funsuite.AnyFunSuite
import wsql.{given, *}

class WsqlTest extends AnyFunSuite {

  import SampleDB.*

  test("jdbc types") {
      dataSource.withStatement { stmt =>
        val rs = stmt.executeQuery("select * from test1")
        while (rs.next) {
          assert(rs.get[Int]("id") == 1)
          assert(rs.get[String]("name") == "test1")
          assert(rs.get[Boolean]("is_active") == true)
          assert(rs.get[Byte]("tiny_int") == 1)
          assert(rs.get[Short]("small_int") == 500)
          assert(rs.get[Int]("normal_int") == 100000)
          assert(rs.get[Long]("big_int") == 4)
          assert(rs.get[Float]("float_value") == 1.1f)
          assert(rs.get[Double]("double_value") == 2.2)
          assert(rs.get[BigDecimal]("decimal_value") == BigDecimal(3.3))
          assert(rs.get[java.sql.Date]("birthday") == java.sql.Date.valueOf("2020-01-01"))
          assert(rs.get[java.sql.Timestamp]("created_at") == java.sql.Timestamp.valueOf("2020-01-01 00:00:00"))
          assert(rs.get[java.sql.Timestamp]("updated_at") == java.sql.Timestamp.valueOf("2020-01-01 00:00:00"))
          assert(rs.get[Array[Byte]]("blob_value").toList == "test".getBytes.toList)
          assert(rs.get[String]("empty_value") == null)
          assert(rs.get[Int]("empty_int") == 0)

          assert( rs.get[Int](1) == 1)
          assert( rs.get[String](2) == "test1")
          assert( rs.get[Boolean](3) == true)
          assert( rs.get[Byte](4) == 1)
          assert( rs.get[Short](5) == 500)
          assert( rs.get[Int](6) == 100000)
          assert( rs.get[Long](7) == 4)
          assert( rs.get[Float](8) == 1.1f)
          assert( rs.get[Double](9) == 2.2)
          assert( rs.get[BigDecimal](10) == BigDecimal(3.3))
          assert( rs.get[java.sql.Date](11) == java.sql.Date.valueOf("2020-01-01"))
          assert( rs.get[java.sql.Timestamp](12) == java.sql.Timestamp.valueOf("2020-01-01 00:00:00"))
          assert( rs.get[java.sql.Timestamp](13) == java.sql.Timestamp.valueOf("2020-01-01 00:00:00"))
          assert( rs.get[Array[Byte]](14).toList == "test".getBytes.toList)
          assert( rs.get[String](15) == null)
          assert( rs.get[Int](16) == 0)

          assert( rs.get[Option[Int]](1) == Some(1))
          assert( rs.get[Option[String]](2) == Some("test1"))
          assert( rs.get[Option[Boolean]](3) == Some(true))
          assert( rs.get[Option[String]](15) == None)
          assert( rs.get[Option[Int]](16) == None)

          assert( rs.get[java.math.BigDecimal](10) == new java.math.BigDecimal("3.30") )

        }

      }

      val row = dataSource.rows[Row]("select * from test1").apply(0)
      assert(row.get[Int]("id") == 1)
      assert(row.get[String]("name") == "test1")
      assert(row.get[Boolean]("is_active") == true)
      assert(row.get[Byte]("tiny_int") == 1)
      assert(row.get[Short]("small_int") == 500)
      assert(row.get[Int]("normal_int") == 100000)
      assert(row.get[Long]("big_int") == 4)
      assert(row.get[Float]("float_value") == 1.1f)
      assert(row.get[Double]("double_value") == 2.2)
      assert(row.get[BigDecimal]("decimal_value") == BigDecimal(3.3))
      assert(row.get[java.sql.Date]("birthday") == java.sql.Date.valueOf("2020-01-01"))
      assert(row.get[java.sql.Timestamp]("created_at") == java.sql.Timestamp.valueOf("2020-01-01 00:00:00"))
      assert(row.get[java.sql.Timestamp]("updated_at") == java.sql.Timestamp.valueOf("2020-01-01 00:00:00"))
      assert(row.get[Array[Byte]]("blob_value").toList == "test".getBytes.toList)
      assert(row.get[String]("empty_value") == null)
      assert(row.get[Int]("empty_int") == 0)

      assert( row.get[Int](1) == 1)
      assert( row.get[String](2) == "test1")
      assert( row.get[Boolean](3) == true)
      assert( row.get[Byte](4) == 1)
      assert( row.get[Short](5) == 500)
      assert( row.get[Int](6) == 100000)
      assert( row.get[Long](7) == 4)
      assert( row.get[Float](8) == 1.1f)
      assert( row.get[Double](9) == 2.2)
      assert( row.get[BigDecimal](10) == BigDecimal(3.3))
      assert( row.get[java.sql.Date](11) == java.sql.Date.valueOf("2020-01-01"))
      assert( row.get[java.sql.Timestamp](12) == java.sql.Timestamp.valueOf("2020-01-01 00:00:00"))
      assert( row.get[java.sql.Timestamp](13) == java.sql.Timestamp.valueOf("2020-01-01 00:00:00"))
      assert( row.get[Array[Byte]](14).toList == "test".getBytes.toList)
      assert( row.get[String](15) == null)
      assert( row.get[Int](16) == 0)

      assert( row.get[Option[Int]](1) == Some(1))
      assert( row.get[Option[String]](2) == Some("test1"))
      assert( row.get[Option[Boolean]](3) == Some(true))
      assert( row.get[Option[String]](15) == None)
      assert( row.get[Option[Int]](16) == None)

      assert( row.get[java.math.BigDecimal](10) == new java.math.BigDecimal("3.30") )

  }


  test("resultset mappers") {

    @UseColumnMapper(classOf[Camel2UnderscoreMapper])
    case class Test1(
        id: Int,
        name: String,
        isActive: Boolean,
        tinyInt: Byte,
        smallInt: Short,
        normalInt: Int,
        bigInt: Long,
        floatValue: Float,
        doubleValue: Double,
        decimalValue: BigDecimal,
        birthday: java.sql.Date,
        createdAt: java.sql.Timestamp,
        updatedAt: java.sql.Timestamp,
        blobValue: Array[Byte],
        emptyValue: Option[String],
        emptyInt: Option[Int])  derives ResultSetMapper

    val mapper = summon[ResultSetMapper[Test1]]
    val rows = dataSource.rows[Test1]("select * from test1")

    assert(rows(0) ==
      Test1(
        id = 1,
        name = "test1",
        isActive = true,
        tinyInt = 1,
        smallInt = 500,
        normalInt = 100000,
        bigInt = 4,
        floatValue = 1.1f,
        doubleValue = 2.2,
        decimalValue = BigDecimal("3.30"),
        birthday = java.sql.Date.valueOf("2020-01-01"),
        createdAt = java.sql.Timestamp.valueOf("2020-01-01 00:00:00"),
        updatedAt = java.sql.Timestamp.valueOf("2020-01-01 00:00:00"),
        blobValue = rows(0).blobValue,  // "test".getBytes,
        emptyValue = None,
        emptyInt = None)
    )
  }

  test("Option field and missed fields process"){
    case class Test2(
        id: Int,
        name: String,
        is_active: Boolean,
        tiny_int2: Option[Byte], // missed field
        small_int2: Short = 10 // missed field
        ) derives ResultSetMapper

    val rows = dataSource.rows[Test2]("select * from test1")
    assert(rows(0) ==
      Test2(
        id = 1,
        name = "test1",
        is_active = true,
        tiny_int2 = None,
        small_int2 = 10) // using default value)
    )
  }

  test("missing field without default") {
    case class Test3(
                      id: Int,
                      name: String,
                      is_active: Boolean,
                      tiny_int2: Option[Byte], // missed field
                      small_int2: Short // missed field
                    ) derives ResultSetMapper

    val caught = intercept[RuntimeException] {
      val rows = dataSource.rows[Test3]("select * from test1")
    }
    assert(caught.getMessage contains "no field small_int2")
  }

}
