package wangzx.scala_commons.sql_test

import wangzx.scala_commons.sql.Table

import scala.beans.BeanProperty

/**
 * Created by wangzx on 15/8/7.
 */
@Table(camelToUnderscore = true)
class User {

  @BeanProperty
  var userName: String = _

  @BeanProperty
  var age: Int = _

  @BeanProperty
  var englishScore: Int = _
}
