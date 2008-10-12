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

package net.liftweb.record

import net.liftweb.util._
import net.liftweb.http.{S, FieldIdentifier, FieldError}
import scala.xml._

/**
  * A simple field that can store and retreive a value of a given type
  */
trait SimpleField extends FieldLocator {
  type SMyType
  type SOwnerType <: Record[SOwnerType]

  private[record] var data: SMyType = _
  private[record] var needsDefault = true
  private[record] var obscured: SMyType = _
  private[record] var fieldName: String = _
  private[record] var errors : List[FieldError] = Nil

  /**
   * Return the owner of this field
   */
  def owner: SOwnerType

  /**
   * Should the field be ignored by the OR Mapper?
   */
  def ignoreField_? = false

  /**
   * The default value of the field
   */
  def defaultValue: SMyType

  /**
   * The text name of this field
   */
  def name: String = fieldName

  /**
   * Convert the field to a String... usually of the form "displayName=value"
   */
  def asString = displayName + "=" + data

  /**
   * Convert the fieldset value to an XHTML representation
   */
  def toXHtml = Text(toString)

  /**
   * The display name of the field (by default, the 'internal' name of the field)
   */
  def displayName = name

  /**
   * Can the value of this field be read without obscuring the result?
   */
  def canRead_? = owner.safe_? || checkCanRead_?

  /**
   * If the owner is not in "safe" mode, check the current environment to see if
   * the field can be read
   */
  def checkCanRead_? = true

  def canWrite_? = owner.safe_? || checkCanWrite_?

  def checkCanWrite_? = true

  def obscure(in: SMyType): SMyType = obscured

  def set(in: SMyType): SMyType = synchronized {
    if (checkCanWrite_?) {
      errors = validators.flatMap(f => f(in))
      if (valid_?) {
        data = in
        needsDefault = false
      }
    }
    data
  }

  def setFromAny(in: Any): SMyType

  def value: SMyType = synchronized {
    if (needsDefault) {data = defaultValue ; needsDefault = false} 

    if (canRead_?) data
    else obscure(data)
  }

  override def toString = value match {
    case null => "null"
    case s => s.toString
  }

  def toForm: NodeSeq

  /**
   * Are we in "safe" mode (i.e., the value of the field can be read or written without any security checks.)
   */
  final def safe_? : Boolean = owner.safe_?

  /**
   * Set the name of this field
   */
  private[record] final def setName_!(newName : String) : String = {
    if(safe_?) fieldName = newName
    fieldName
  }

  def valid_? = errors == Nil

  def clearMessages = errors = Nil

  def validationErrors = errors

  def validators : List[SMyType => List[FieldError]] = Nil
}

trait Field[MyType, OwnerType <: Record[OwnerType]] extends SimpleField with FieldIdentifier {
  type SMyType = MyType
  type SOwnerType = OwnerType

  def apply(in: SMyType): OwnerType = if (owner.meta.mutable_?) {
    this.set(in)
    owner
  } else {
    owner.meta.createWithMutableField(owner, this, in)
  }

}

trait JDBCField[MyType, OwnerType <: Record[OwnerType]] extends Field[MyType, OwnerType]{
   def jdbcFriendly(field : String) : MyType
}

trait KeyField[MyType, OwnerType <: Record[OwnerType] with KeyedRecord[OwnerType, MyType]] extends Field[MyType, OwnerType] {
  def ===(other: KeyField[MyType, OwnerType]): Boolean = this.value == other.value
}

trait FieldHandler

trait FieldHandlerRequest[RetType <: FieldHandler]

object JdbcHandler extends JdbcFieldHandler
class JdbcFieldHandler extends FieldHandler
object XmlHandler extends XmlHandlerClass
class XmlHandlerClass extends FieldHandler
object JdbcRequest extends FieldHandlerRequest[JdbcFieldHandler]
object XmlRequest extends FieldHandlerRequest[XmlHandlerClass]

trait FieldLocator { self: SimpleField =>
  def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] = Empty
}

trait JdbcLocator extends FieldLocator { self: SimpleField =>
override def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] =
  request match {
    case JdbcRequest => Full(JdbcHandler)
    case _ => super.locateFieldHandler(request)
  }
}

trait XmlLocator extends FieldLocator { self: SimpleField =>
override def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] =
  request match {
    case XmlRequest => Full(XmlHandler)
    case _ => super.locateFieldHandler(request)
  }
}
