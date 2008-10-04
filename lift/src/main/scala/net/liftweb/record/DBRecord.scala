package net.liftweb.record

import net.liftweb._
import util._
import scala.xml._
import net.liftweb.mapper.{ConnectionIdentifier, Safe, DefaultConnectionIdentifier, DB, KeyObfuscator}


trait DBRecord[MyType <: Record[MyType] with DBRecord[MyType]] {
  self: MyType =>
  
  private var dbConnectionIdentifier:Can[ConnectionIdentifier] = Empty
    
  /**
   * The meta record (the object that contains the meta result for this type)
   */
  def meta: DBMetaRecord[MyType]
  
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


trait DBMetaRecord[BaseRecord <: DBRecord[BaseRecord]] {
  self: BaseRecord =>
    def dbDefaultConnectionIdentifier: ConnectionIdentifier = DefaultConnectionIdentifier
}