package net.liftweb.record.field

import net.liftweb.http.{S, FieldError}
import net.liftweb.util._
import scala.xml._
import S._

trait NumericField[MyType, OwnerType <: Record[OwnerType]] extends Field[MyType, OwnerType] {
  /**
   * Returns form input of this field
   */
  override def toForm = <input type="text" name={S.mapFunc{s: List[String] => {
    this.setFromAny(s) match {
      case Empty => valueCouldNotBeSet = true
      case _ => valueCouldNotBeSet = false
    }}}} value={value.toString}/>

   override def errorMessage = S.??("number.required")
}
