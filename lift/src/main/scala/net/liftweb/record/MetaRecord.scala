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
   
  private[record] var fieldList: List[FieldHolder[BaseRecord]] = Nil

  private[record] var lifecycleCallbacks: List[(String, Method)] = Nil
  
  protected val rootClass = this.getClass.getSuperclass

  private def isMagicObject(m: Method) = m.getReturnType.getName.endsWith("$"+m.getName+"$") && m.getParameterTypes.length == 0
  private def isMappedField(m: Method) = classOf[Field[Nothing, BaseRecord]].isAssignableFrom(m.getReturnType)
  private def isLifecycle(m: Method) = classOf[LifecycleCallbacks].isAssignableFrom(m.getReturnType)

  def introspect(rec: BaseRecord, methods: Array[Method])(f: (Method, Field[_, BaseRecord]) => Any) = {
    for (v <- methods  if isMagicObject(v) && isMappedField(v)) {
      v.invoke(rec, null) match {
        case mf: Field[_, BaseRecord] if !mf.ignoreField_? =>
          mf.setName_!(v.getName)
          f(v, mf)
        case _ =>
      }
    }
    
  }
  
  this.runSafe {
    val tArray = new ListBuffer[FieldHolder[BaseRecord]]
    
    lifecycleCallbacks = for (v <- this.getClass.getSuperclass.getMethods.toList if isMagicObject(v) && isLifecycle(v)) yield (v.getName, v)
    
    introspect(this, this.getClass.getSuperclass.getMethods) {
      case (v, mf) => tArray += FieldHolder(mf.name, v, mf)
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

  def mutable_? = true
  
  /**
   * Creates a mew record
   */
  def createRecord: BaseRecord = {
    val rec: BaseRecord = rootClass.newInstance.asInstanceOf[BaseRecord]
    rec.runSafe {
      introspect(rec, rec.getClass.getMethods) {case (v, mf) =>}
    }
    rec
  }
  
  /**
   * Creates a new record setting the value of the fields from the original object but
   * apply the new value for the specific field
   * 
   * @param - original the initial record
   * @param - field the new mutated field
   * @param - the new value of the field
   */
  def createWithMutableField[FieldType](original: BaseRecord, 
                                        field: Field[FieldType, BaseRecord], 
                                        newValue: FieldType): BaseRecord = {
    val rec = createRecord
  
    for (f <- fieldList) {
      (f.name == field.name) match {
        case true => rec.fieldByName(f.name) match {
          case Full(fld) => fld.setFromAny(newValue) 
          case _ =>
        }
        case false => rec.fieldByName(f.name) match {
          case Full(field) => 
            original.fieldByName(f.name) match {
              case Full(origField) => field.setFromAny(origField.value)
              case _ => 
            }
          case _ =>
        }
      }
    }
    
    rec
  }
  
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
  
  def fieldOrder: List[Field[_, BaseRecord]] = Nil
   

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

