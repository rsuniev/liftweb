/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.builtin.snippet

import _root_.net.liftweb.http.{S, DispatchSnippet, LiftRules}
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml._

/**
 * <p>This built-in snippet can be used to render a menu representing your SiteMap.
 * There are three main snippet methods that you can use:</p>
 *
 * <ul>
 *   <li>builder - Renders the entire SiteMap, optionally expanding all child menus</li>
 *   <li>group - Renders the MenuItems corresponding to the specified group.</li>
 *   <li>item - Renders the specific named MenuItem</li>
 * </ul>
 *
 * <p>More detailed usage of each method is provided below</p>
 */
object Menu extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case "builder" => builder
    case "title" => title
    case "item" => item
    case "group" => group
    case "json" => jsonMenu
  }

  /**
   * <p>This snippet method renders a menu representing your SiteMap contents. The
   * menu is rendered as a set of nested unordered lists (&lt;ul /&gt;). By
   * default, it only renders nested child menus for menus that match the current request path.
   * You can add the "expandAll" attribute to the snippet tag to force full expansion of
   * all child menus. Additionally, you can use the following attribute prefixes to further customize
   * the generated list and list item elements:</p>
   *
   * <ul>
   *   <li>top - Adds the specified attribute to the top-level &lt;ul&gt; element that makes up the menu</li>
   *   <li>ul - Adds the specified attribute to each &lt;ul&gt; element (top-level and nested children) that makes up the menu</li>
   *   <li>li - Adds the specified attribute to each &lt;li&gt; element for the menu</li>
   *   <li>li_item - Adds the specified attribute to the current page’s menu item</li>
   *   <li>outer_tag - the tag for the outer XML element (ul by default)</li>
   *   <li>inner_tag - the tag for the inner XML element (li by default)</li>
   *   <li>li_path - Adds the specified attribute to the current page’s breadcrumb path. The
   *       breadcrumb path is the set of menu items leading to this one.</li>
   * </ul>
   *
   * <p>For a simple, default menu, simply add</p>
   *
   * <pre>
   *   &lt;lift:Menu.builder /&gt;
   * </pre>
   *
   * <p>To your template. You can render the entire sitemap with</p>
   *
   * <pre>
   *    &lt;lift:Menu.builder expandAll="true" /&gt;
   * </pre>
   *
   * <p>Customizing the elements is handled through the prefixed attributes described above.
   *    For instance, you could make the current page menu item red:</p>
   *
   * <pre>
   *    &lt;lift:Menu.builder li_item:style="color: red;" /&gt;
   * </pre>
   */
  def builder(info: NodeSeq): NodeSeq = {
    val outerTag: String = S.attr("outer_tag") openOr "ul"
    val innerTag: String = S.attr("inner_tag") openOr "li"
    val expandAll = S.attr("expandAll").isDefined
    val toRender = renderWhat(expandAll)

    toRender.toList match {
      case Nil => Text("No Navigation Defined.")
      case xs =>
        val liMap = S.prefixedAttrsToMap("li")
        val li = S.mapToAttrs(liMap)

        def buildANavItem(i: MenuItem) = {
          i match {
            case MenuItem(text, uri, kids, true, _, _) if expandAll =>
              Elem(null, innerTag, Null, TopScope,
                   <xml:group><a href={uri}>{text}</a>{buildUlLine(kids)}</xml:group>) %
              S.prefixedAttrsToMetaData("li_item", liMap)
              //(<li><a href={uri}>{text}</a>{buildUlLine(kids)}</li>) % S.prefixedAttrsToMetaData("li_item", liMap)

            case MenuItem(text, uri, kids, true, _, _) =>
              Elem(null, innerTag, Null, TopScope, 
                   <xml:group><span>{text}</span>{buildUlLine(kids)}</xml:group>) %
              S.prefixedAttrsToMetaData("li_item", liMap)

            case MenuItem(text, uri, kids,  _, true, _) =>
              Elem(null, innerTag, Null, TopScope, 
                   <xml:group><a href={uri}>{text}</a>{buildUlLine(kids)}</xml:group>) %
              S.prefixedAttrsToMetaData("li_path", liMap)

            case MenuItem(text, uri, kids, _, _, _) =>
              Elem(null, innerTag, Null, TopScope, 
                   <xml:group><a href={uri}>{text}</a>{buildUlLine(kids)}</xml:group>) % li
          }
        }

        def buildUlLine(in: Seq[MenuItem]) : NodeSeq =
        if (in.isEmpty) {
          Text("")
        } else {
          Elem(null, outerTag, Null, TopScope,
               <xml:group>{in.flatMap(buildANavItem)}</xml:group>) %
          S.prefixedAttrsToMetaData("ul")
        }

        buildUlLine(xs) match {
          case top : Elem => top % S.prefixedAttrsToMetaData("top")
          case other => other
        }
    }
  }

  private def renderWhat(expandAll: Boolean): Seq[MenuItem] =
      (if (expandAll) for {sm <- LiftRules.siteMap; req <- S.request} yield
       sm.buildMenu(req.location).lines
       else S.request.map(_.buildMenu.lines)) openOr Nil

  def jsonMenu(ignore: NodeSeq): NodeSeq = {
    val toRender = renderWhat(true)

    def buildItem(in: MenuItem): JsExp =  in match {
      case MenuItem(text, uri, kids, current, path, _) =>
        JsObj("text" -> text.toString,
              "uri" -> uri.toString,
              "children" -> buildItems(kids),
              "current" -> current,
              "path" -> path)
    }

    def buildItems(in: Seq[MenuItem]): JsExp =
    JsArray(in.map(buildItem) :_*)

    Script(JsCrVar(S.attr("var") openOr "lift_menu", 
                   JsObj("menu" -> buildItems(toRender))))
  }


  /**
   * <p>Renders the title for the current request path (location). You can use this to
   * automatically set the title for your page based on your SiteMap:</p>
   *
   * <pre>
   * ...
   * &lt;head&gt;
   *   &lt;title&gt;&lt;lift:Menu.title /&gt;&lt;/title&gt;
   * &lt;/head&gt;
   * ...
   * </pre>
   */
  def title(text: NodeSeq): NodeSeq = {
    val r =
    for (request <- S.request;
         loc <- request.location) yield loc.title
    r openOr Text("")
  }

  /**
   * <p>Renders a group of menu items. You specify a group using the LocGroup LocItem
   * case class on your Menu Loc:</p>
   *
   * <pre>
   * val menus =
   *   Menu(Loc("a",...,...,LocGroup("test"))) ::
   *   Menu(Loc("b",...,...,LocGroup("test"))) ::
   *   Menu(Loc("c",...,...,LocGroup("test"))) :: Nil
   * </pre>
   *
   * <p>You can then render with the group snippet:</p>
   *
   * <pre>
   * &lt;lift:Menu.group group="test" /&gt;
   * </pre>
   *
   * <p>Each menu item is rendered as an anchor tag (&lta /&gt;), and you can customize
   * the tag using attributes prefixed with "a":</p>
   *
   * <pre>
   * &lt;lift:Menu.group group="test" a:class="menulink" /&gt;
   * </pre>
   *
   * <p>You can also specify your own template within the Menu.group snippet tag, as long as
   * you provide a &lt;menu:bind /&gt; element where the snippet can place each menu item:</p>
   *
   * <pre>
   * &lt;ul&gt;
   * &lt;lift:Menu.group group="test" &gt;
   *   &lt;li&gt;&lt;menu:bind /&gt;&lt;/li&gt;
   * &lt;/lift:Menu.group&gt;
   * </pre>
   *
   */
  def group(template: NodeSeq): NodeSeq = {
    val toBind = if ((template \ "bind").filter(_.prefix == "menu").isEmpty)
    <xml:group><menu:bind/> </xml:group>
    else template

    val attrs = S.prefixedAttrsToMetaData("a")

    for (group <- S.attr("group").toList;
         siteMap <- LiftRules.siteMap.toList;
         loc <- siteMap.locForGroup(group);
         link <- loc.createDefaultLink;
         linkText <- loc.linkText) yield {
      val a = <a href={link}>{linkText}</a> % attrs

      Group(bind("menu", toBind, "bind" -> a))
    }
  }

  /**
   * <p>Renders a specific, named item, based on the name given in the Menu's Loc paramter:</p>
   *
   * <pre>
   * val menus =
   *   Menu(Loc("a",...,...,LocGroup("test"))) ::
   *   Menu(Loc("b",...,...,LocGroup("test"))) ::
   *   Menu(Loc("c",...,...,LocGroup("test"))) :: Nil
   * </pre>
   *
   * <p>You can then select the item using the name attribute:</p>
   *
   * <pre>
   * &lt;lift:Menu.item name="b" /&gt;
   * </pre>
   *
   * <p>The menu item is rendered as an anchor tag (&lta /&gt;). The text for the link
   * defaults to the named Menu's Loc.linkText, but you can specify your own link text
   * by providing contents to the tag:</p>
   *
   * <pre>
   * &lt;lift:Menu.item name="b"&gt;This is a link&lt;/lift:Menu.item&gt;
   * </pre>
   *
   * <p>Additionally you can customize
   * the tag using attributes prefixed with "a":</p>
   *
   * <pre>
   * &lt;lift:Menu.item name="b" a:style="color: red;" /&gt;
   * </pre>
   *
   * <p>Normally, the Menu item is not shown on pages that match its Menu's Loc. You can
   * set the "donthide" attribute on the tag to force it to show text only (same text as normal,
   * but not in an anchor tag)</p>
   *
   */
  def item(text: NodeSeq): NodeSeq =
  for (name <- S.attr("name").toList;
       request <- S.request.toList;
       loc <- request.location.toList)
  yield {
    if (loc.name != name) {
      val itemLink = SiteMap.buildLink(name, text) match {
        case e : Elem => e % S.prefixedAttrsToMetaData("a")
        case x => x
      }
      Group(itemLink)
    } else if (S.attr("donthide").isDefined) {
      // Use the provided text if it's non-empty, otherwise, default to Loc's LinkText
      if (text.length > 0) {
        Group(text)
      } else {
        Group(loc.linkText openOr Text(loc.name))
      }
    } else {
      Text("")
    }
  }

}
