package net.liftweb.record

import net.liftweb._
import util._
import scala.collection.mutable.{ListBuffer}
import scala.xml._
import net.liftweb.http.js.{JsExp, JE}
import net.liftweb.http.{FieldError, SHtml}
import net.liftweb.mapper.{Safe, KeyObfuscator}
import java.lang.reflect.Method
import field._

/**
 * Holds meta information and operations on a record
 */
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

