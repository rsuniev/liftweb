package net.liftweb.record.field

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.{S}
import _root_.java.util.regex._
import Helpers._
import S._


object EmailField {
  val emailPattern = Pattern.compile("^[a-z0-9._%-]+@(?:[a-z0-9-]+\\.)+[a-z]{2,4}$")
  def validEmailAddr_?(email: String): Boolean = emailPattern.matcher(email).matches
}

class EmailField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends StringField[OwnerType](rec, maxLength) {

  def this(rec: OwnerType, maxLength: Int, value: String) = {
    this(rec, maxLength)
    set(value)
  }

  def this(rec: OwnerType, value: String) = {
    this(rec, 100)
    set(value)
  }

  private def validateEmail(email: String): Can[List[Node]] = EmailField.validEmailAddr_?(email) match {
    case false => Full(Text(S.??("invalid.email.address")) :: Nil)
    case _ => Empty
  }

  override def validators = validateEmail _ :: Nil

}


import java.sql.{ResultSet, Types}
import net.liftweb.mapper.{DriverType}

/**
 * An email field holding DB related logic
 */
abstract class DBEmailField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType, maxLength: Int) extends
  EmailField[OwnerType](rec, maxLength) with JDBCField[String, OwnerType]{

  def this(rec: OwnerType, maxLength: Int, value: String) = {
    this(rec, maxLength)
    set(value)
  }

  def this(rec: OwnerType, value: String) = {
    this(rec, 100)
    set(value)
  }

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" VARCHAR("+maxLength+")"

  def jdbcFriendly(field : String) : String = value
}
