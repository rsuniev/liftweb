package net.liftweb.record

/* 
* Copyright 2007-2008 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License"); 
* you may not use this file except in compliance with the License. 
* You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 
* Unless required by applicable law or agreed to in writing, software 
* distributed under the License is distributed on an "AS IS" BASIS, 
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
* See the License for the specific language governing permissions and
* limitations under the License.
*/

import net.liftweb._
import util._
import scala.xml._
import net.liftweb.mapper.{ConnectionIdentifier, Safe, DefaultConnectionIdentifier, DB, KeyObfuscator}
import net.liftweb.http.js.{JsExp}
import net.liftweb.http.{FieldError, SHtml}
     
trait Record[MyType <: Record[MyType]] {
  self: MyType =>
  
  /**
   * A unique identifier for this record... used for access control
   */
  private val secure_# = Safe.next

  /**
   * Was this instance deleted from backing store?
   */
  private var was_deleted_? = false
  
  /**
   * The meta record (the object that contains the meta result for this type)
   */
  def meta: MetaRecord[MyType]
  
  /**
   * Is it safe to make changes to the record (or should we check access control?)
   */
  final def safe_? : boolean = {
    Safe.safe_?(secure_#)
  }
  
  /**
   * Save the instance and return the instance
   */
  def save(): MyType = {
    runSafe {
      meta.save(this)
    }
    this
  }
  
  def runSafe[T](f : => T) : T = {
    Safe.runSafe(secure_#)(f)
  }
  
  def htmlLine: NodeSeq = {
    meta.doHtmlLine(this)
  }
  
  def asHtml: NodeSeq = {
    meta.asHtml(this)
  }
  
  /**
  * If the instance calculates any additional
  * fields for JSON object, put the calculated fields
  * here
  */
  def suplementalJs(ob: Can[KeyObfuscator]): List[(String, JsExp)] = Nil
  
  def validate : List[FieldError] = {
    runSafe {
      meta.validate(this)
    }
  }
  
  def asJs: JsExp = {
    meta.asJs(this)
  }
  
  /**
   * Delete the instance from backing store
   */
  def delete_! : Boolean = {
    if (!db_can_delete_?) false else
    runSafe {
      was_deleted_? = meta.delete_!(this)
      was_deleted_?
    }
  }
  
  /**
   * Can this model object be deleted?
   */
  def db_can_delete_? : Boolean =  meta.saved_?(this) && !was_deleted_?
  
  /**
   * Present the model as a form and execute the function on submission of the form
   *
   * @param button - If it's Full, put a submit button on the form with the value of the parameter
   * @param f - the function to execute on form submission
   *
   * @return the form
   */
  def toForm(f: MyType => Unit): NodeSeq =
  meta.toForm(this) ++ (SHtml.hidden(() => f(this)))
  
  /**
   * Find the field by name
   * @param fieldName -- the name of the field to find
   *
   * @return Can[MappedField]
   */ 
  def fieldByName[T](fieldName: String): Can[Field[T, MyType]] = meta.fieldByName[T](fieldName, this)  
}

trait ExpandoRecord[MyType <: Record[MyType] with ExpandoRecord[MyType]] {
  self: MyType =>

  /**
   * If there's a field in this record that defines the locale, return it
   */
  def localeField: Can[LocaleField[MyType]] = Empty
  
  def timeZoneField: Can[TimeZoneField[MyType]] = Empty
  
  def countryField: Can[CountryField[MyType]] = Empty
}

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

trait KeyedRecord[MyType <: KeyedRecord[MyType, KeyType] with Record[MyType], KeyType] {
  self: MyType =>
  
  def primaryKey: KeyField[KeyType, MyType]
  
  def comparePrimaryKeys(other: MyType) = primaryKey === other.primaryKey
}

trait MetaRecord[BaseRecord <: Record[BaseRecord]] { self: BaseRecord =>
  /**
   * Convert the name and value of a field to a String.  Override this method
   * to change its default behavior
   */
  def fieldToString(name: String, strValue: String) = name+"="+strValue
  
  def fieldToXHtml(name: String, strValue: NodeSeq) = <xml:group><td>{name}</td><td>{strValue}</td></xml:group>
  
  def mutable_? = true
      
  def createWithMutatedField[FieldType](original: BaseRecord, field: Field[FieldType, BaseRecord], newValue: FieldType): BaseRecord = null.asInstanceOf[BaseRecord] // FIXME
  
  /**
   * Save the instance in the appropriate backing store
   */
  def save(inst: BaseRecord): Boolean
  
  /**
   * Was this instance saved in backing store?
   */
  def saved_?(inst: BaseRecord): Boolean
  
  /**
   * Delete the instance from backing store
   */
  def delete_!(inst: BaseRecord): Boolean
  
  def doHtmlLine(inst: BaseRecord): NodeSeq
  
  def asHtml(inst: BaseRecord): NodeSeq
  
  def validate(inst: BaseRecord): List[FieldError]
  
  def asJs(inst: BaseRecord): JsExp
  
  def toForm(inst: BaseRecord): NodeSeq
  
  /**
   * Get a field by the field name
   * @param fieldName -- the name of the field to get
   * @param actual -- the instance to get the field on
   *
   * @return Can[The Field] (Empty if the field is not found)
   */
  def fieldByName[T](fieldName: String, inst: BaseRecord): Can[Field[T, BaseRecord]]
}

trait DBMetaRecord[BaseRecord <: DBRecord[BaseRecord]] {
  self: BaseRecord =>
    def dbDefaultConnectionIdentifier: ConnectionIdentifier = DefaultConnectionIdentifier
}

