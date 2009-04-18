package net.liftweb.http

import _root_.net.liftweb.util.{Box, Full, SoftReferenceCache}
import _root_.scala.xml.{NodeSeq}

trait TemplateCache {

  /**
   * Returns a cached template by name. If the template is not cached yet,
   * it will be provided by templateProvicer.
   */
  def findTemplate(name: String)(templateProvicer : => Box[NodeSeq]): Box[NodeSeq];
  
  /**
   * Removes a template from the cache
   */
  def removeTemplate(name: String);
}

/**
 * A cache that caches nothing
 */
object NoCache extends TemplateCache {

  def findTemplate(name: String)(provider : => Box[NodeSeq]): Box[NodeSeq] = {
	provider
  }

  override def removeTemplate(name: String) {
  }
}

/**
 * Companion module for InMemoryCache
 */
object InMemoryCache {
  SoftReferenceCache.initialize
  LiftRules.unloadHooks.prepend {() => 
    SoftReferenceCache.shutDown
  }
  def apply(templatesCount: Int) = new InMemoryCache(templatesCount)
}

/**
 * Caches templates in a LRU map
 */
private[http] class InMemoryCache(templatesCount: Int) extends TemplateCache {

  private val cache = new SoftReferenceCache[String, Box[NodeSeq]](templatesCount)

  def findTemplate(name: String)(provider : => Box[NodeSeq]): Box[NodeSeq] = {
    cache(name) openOr {
      val template = provider;
      template map (node => {
        cache += (name -> template)
      })
      template
    }
  }

  override def removeTemplate(name: String) {
    cache.remove(name)
  }

}
