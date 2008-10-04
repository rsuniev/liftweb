package net.liftweb.record

trait BackendRecord[MyType <: Record[MyType]] extends Record[MyType]{ self: MyType =>
  
  /**
   * Was this instance deleted from backing store?
   */
  private var was_deleted_? = false

  /**
   * The meta record (the object that contains the meta result for this type)
   */
  def meta: BackendMetaRecord[MyType]

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

}
