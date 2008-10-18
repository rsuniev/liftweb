package net.liftweb.record.field

import net.liftweb.http.{S, FieldError}
import net.liftweb.util._
import scala.xml._
import S._
import Helpers._

trait NumericField[MyType, OwnerType <: Record[OwnerType]] extends Field[MyType, OwnerType] {
  /**
   * Returns form input of this field
   */
  override def toForm = {
    var el = <input type="text" name={S.mapFunc{s: List[String] => {
      this.setFromAny(s) match {
        case Empty => valueCouldNotBeSet = true
        case _ => valueCouldNotBeSet = false
      }}}} value={value.toString}/>

    uniqueFieldId match {
      case Full(id) =>
        <div id={name+"_div_id"}><div><label for={id}>{displayName}</label></div>{(el % ("id" -> id))}</div>
      case _ => <div>{el}</div>
    }

  }

  override def errorMessage = S.??("number.required")
}
