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
import scala.collection.mutable.{ListBuffer, HashMap}
import scala.xml._
import net.liftweb.http.js.{JsExp, JE}
import net.liftweb.http.{FieldError, SHtml}
import net.liftweb.mapper.{Safe, KeyObfuscator}
import java.lang.reflect.Method
import field._
     
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


trait KeyedRecord[MyType <: KeyedRecord[MyType, KeyType] with Record[MyType], KeyType] {
  self: MyType =>
  
  def primaryKey: KeyField[KeyType, MyType]
  
  def comparePrimaryKeys(other: MyType) = primaryKey === other.primaryKey
}

trait MetaRecord[BaseRecord <: Record[BaseRecord]] { self: BaseRecord =>
   
  def mutable_? = true
      
  def createWithMutableField[FieldType](original: BaseRecord, field: Field[FieldType, BaseRecord], newValue: FieldType): BaseRecord = null.asInstanceOf[BaseRecord] // FIXME
  
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
  
  def asHtml(inst: BaseRecord): NodeSeq = NodeSeq.Empty
  
  def validate(toValidate: BaseRecord): List[FieldError] = Nil // TODO - implement this  
  
  def asJs(inst: BaseRecord): JsExp = JE.JsObj(("$lift_class", JE.Str("temp"))) // TODO - implement this
  
  def toForm(inst: BaseRecord): NodeSeq = <form></form> // TODO - implement this

  private[record] def ??(meth: Method, inst: BaseRecord) = meth.invoke(inst, null).asInstanceOf[Field[_, BaseRecord]]
  
  /**
   * Get a field by the field name
   * @param fieldName -- the name of the field to get
   * @param actual -- the instance to get the field on
   *
   * @return Can[The Field] (Empty if the field is not found)
   */
  def fieldByName[T](fieldName: String, inst: BaseRecord): Can[Field[T, BaseRecord]] = {
    Can(fieldList.find(f => f.name == fieldName)).map(holder => ??(holder.method, inst).asInstanceOf[Field[T, BaseRecord]])
  }
  
  private[record] var fieldList: List[FieldHolder[BaseRecord]] = Nil;
  
  private var lifecycleCallbacks: List[(String, Method)] = Nil

  def fieldOrder: List[Field[_, BaseRecord]] = Nil
   
  this.runSafe {
    val tArray = new ListBuffer[FieldHolder[BaseRecord]]
    
    def isMagicObject(m: Method) = m.getReturnType.getName.endsWith("$"+m.getName+"$") && m.getParameterTypes.length == 0
    def isMappedField(m: Method) = classOf[Field[Nothing, BaseRecord]].isAssignableFrom(m.getReturnType)
    def isLifecycle(m: Method) = classOf[LifecycleCallbacks].isAssignableFrom(m.getReturnType)
    
    lifecycleCallbacks = for (v <- this.getClass.getSuperclass.getMethods.toList if isMagicObject(v) && isLifecycle(v)) yield (v.getName, v)
    
    for (v <- this.getClass.getSuperclass.getMethods  if isMagicObject(v) && isMappedField(v)) {
      v.invoke(this, null) match {
        case mf: Field[_, BaseRecord] if !mf.ignoreField_? =>
          mf.owner = this;
          mf.setName_!(v.getName)
          tArray += FieldHolder(mf.name, v, mf)
        case _ =>
      }
    }
    
    def findPos(in: AnyRef) : Can[Int] = {
      tArray.toList.zipWithIndex.filter(mft => in eq mft._1.field) match {
        case Nil => Empty
        case x :: xs => Full(x._2)
      }
    }
    
    val resArray = new ListBuffer[FieldHolder[BaseRecord]];
    
    fieldOrder.foreach(f => findPos(f).foreach(pos => resArray += tArray.remove(pos)))
    
    tArray.foreach(mft => resArray += mft)      
    
    fieldList = resArray.toList
  }

  private def eachField(what: BaseRecord, toRun: List[(BaseRecord) => Any])(f: (LifecycleCallbacks) => Any) {
    lifecycleCallbacks.foreach (e =>
      e._2.invoke(what, null) match {
        case lccb: LifecycleCallbacks => f(lccb)
        case _ =>
      })
    toRun.foreach{tf => tf(what)}
  }
  
  def beforeValidation: List[BaseRecord => Any] = Nil
  def beforeValidationOnCreate: List[BaseRecord => Any] = Nil
  def beforeValidationOnUpdate: List[BaseRecord => Any] = Nil
  def afterValidation: List[BaseRecord => Any] = Nil
  def afterValidationOnCreate: List[BaseRecord => Any] = Nil
  def afterValidationOnUpdate: List[BaseRecord => Any] = Nil

  def beforeSave: List[BaseRecord => Any] = Nil
  def beforeCreate: List[(BaseRecord) => Any] = Nil
  def beforeUpdate: List[(BaseRecord) => Any] = Nil

  def afterSave: List[(BaseRecord) => Any] = Nil
  def afterCreate: List[(BaseRecord) => Any] = Nil
  def afterUpdate: List[(BaseRecord) => Any] = Nil

  def beforeDelete: List[(BaseRecord) => Any] = Nil
  def afterDelete: List[(BaseRecord) => Any] = Nil

  
  case class FieldHolder[T](name: String, method: Method, field: Field[_, T]) 
}


trait LifecycleCallbacks {
  def beforeValidation {}
  def beforeValidationOnCreate {}
  def beforeValidationOnUpdate {}
  def afterValidation {}
  def afterValidationOnCreate {}
  def afterValidationOnUpdate {}

  def beforeSave {}
  def beforeCreate {}
  def beforeUpdate {}

  def afterSave {}
  def afterCreate {}
  def afterUpdate {}

  def beforeDelete {}
  def afterDelete {}
}
