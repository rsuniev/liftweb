package net.liftweb.record

import net.liftweb._
import util._
import scala.xml._
import net.liftweb.mapper.{ConnectionIdentifier, Safe, DefaultConnectionIdentifier, DB, KeyObfuscator, SuperConnection, QueryParam}
import java.sql.{ResultSet, Types, PreparedStatement, Statement}

trait DBMetaRecord[BaseRecord <: DBRecord[BaseRecord]] extends MetaRecord[BaseRecord] {
  self: BaseRecord =>

  /**
   * Save the instance in the appropriate backing store
   */
  def save(inst: BaseRecord): Boolean = true

  /**
   * Was this instance saved in backing store?
   */
  def saved_?(inst: BaseRecord): Boolean = true

  /**
   * Delete the instance from backing store
   */
  def delete_!(inst: BaseRecord): Boolean = true

  def dbDefaultConnectionIdentifier: ConnectionIdentifier = DefaultConnectionIdentifier

  def afterCommit: List[BaseRecord => Unit] = Nil

  // To be continued with DB related stuff
}
