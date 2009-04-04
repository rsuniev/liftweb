package net.liftweb.http

import _root_.net.liftweb.util.{Box, Full, LRU}
import _root_.scala.xml.{NodeSeq}

trait TemplateCache {

  /**
   * Returns a cached template by name. If the template is not cached yet,
   * it will be provided by templateProvicer.
   */
  def findTemplate(name: String)(templateProvicer : => Box[NodeSeq]): Box[NodeSeq];
}

/**
 * A cache that caches nothing
 */
object NoCache extends TemplateCache {

  def findTemplate(name: String)(provider : => Box[NodeSeq]): Box[NodeSeq] = {
	provider
  }

}

object InMemoryCache {
  def apply(templatesCount: Int) = new InMemoryCache(templatesCount)
}

/**
 * Caches templates in a LRU map
 */
class InMemoryCache(templatesCount: Int) extends TemplateCache {

  private val cache = new LRU[String, Box[NodeSeq]](templatesCount)

  def findTemplate(name: String)(provider : => Box[NodeSeq]): Box[NodeSeq] = {
    Box !!(cache(name)) openOr {
      val template = provider;
      template map (node => {
        cache.update(name, template)
      })
      template
    }
  }
}
