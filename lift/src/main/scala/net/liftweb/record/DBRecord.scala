package net.liftweb.record

import net.liftweb._
import util._
import scala.xml._
import net.liftweb.mapper.{ConnectionIdentifier, Safe, DefaultConnectionIdentifier, DB, KeyObfuscator}


trait DBRecord[MyType <: DBRecord[MyType]] extends Record[MyType] {
  self: MyType =>

  /**
   * Was this instance deleted from backing store?
   */
  private var was_deleted_? = false

  /**
   * The meta record (the object that contains the meta result for this type)
   */
  def meta: DBMetaRecord[MyType]

  /**
   * Save the instance and return the instance
   */
  def save(): MyType = {
    runSafe {
      meta.save(this)
    }
    this
  }

  /**
   * Delete the instance from backing store
   */
  def delete_! : Boolean = {
    if (!can_delete_?) false else
    runSafe {
      was_deleted_? = meta.delete_!(this)
      was_deleted_?
    }
  }

  /**
   * Can this model object be deleted?
   */
  def can_delete_? : Boolean =  meta.saved_?(this) && !was_deleted_?

  private var dbConnectionIdentifier : Can[ConnectionIdentifier] = Empty

  def connectionIdentifier = dbConnectionIdentifier openOr calcDbId

  def dbCalculateConnectionIdentifier: PartialFunction[MyType, ConnectionIdentifier] = Map.empty

  private def calcDbId = if (dbCalculateConnectionIdentifier.isDefinedAt(this)) dbCalculateConnectionIdentifier(this)
                         else meta.dbDefaultConnectionIdentifier

  /**
   * Append a function to perform after the commit happens
   * @param func - the function to perform after the commit happens
   */
  def doPostCommit(func: () => Unit) {
    DB.appendPostFunc(connectionIdentifier, func)
  }
}
