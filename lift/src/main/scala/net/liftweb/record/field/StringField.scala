package net.liftweb.record.field

import scala.xml._
import net.liftweb.util._
import net.liftweb.http.{S}
import S._

/**
 * A Field containing String content.
 */
abstract class StringField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends Field[String, OwnerType] {
  
  def this(rec: OwnerType, maxLength: Int, value: String) = {
    this(rec, maxLength)
    set(value)
  } 

  def this(rec: OwnerType, value: String) = {
    this(rec, -1)
    set(value)
  } 
  
  override def fromString(in: String) = Full(in)
  
  def owner = rec
  
  override def setFromAny(in: Any) {
    in match {
      case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
      case (s: String) :: _ => this.set(s)
      case null => this.set(null)
      case s: String => this.set(s)
      case Some(s: String) => this.set(s)
      case Full(s: String) => this.set(s)
      case None | Empty | Failure(_, _, _) => this.set(null)
      case o => this.set(o.toString)
    }
  }

  
  override def toForm = <input type="text" maxlength={maxLength.toString} 
	 name={S.mapFunc({s: List[String] => this.setFromAny(s)})} 
	 value={value match {case null => "" case s => s.toString}}/> 
  
  override def defaultValue = ""
  
}


trait TimeZoneField[OwnerType <: Record[OwnerType]] extends StringField[OwnerType] {

}

trait CountryField[OwnerType <: Record[OwnerType]] extends StringField[OwnerType] {

}

trait LocaleField[OwnerType <: Record[OwnerType]] extends StringField[OwnerType] {

}

