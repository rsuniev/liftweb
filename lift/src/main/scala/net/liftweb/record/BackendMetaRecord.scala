package net.liftweb.record

trait BackendMetaRecord[BaseRecord <: Record[BaseRecord]] extends MetaRecord[BaseRecord] { self: BaseRecord =>
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

  
}
