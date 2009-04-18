/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
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
package net.liftweb.builtin.snippet;

import _root_.net.liftweb.http._
import S._
import _root_.scala.xml._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util.{Box, Full, Empty}

/**
 * This class is a built in snippet that renders the messages (Errors, Warnings, Notices). Typically it is used in templates
 * as a place holder for any messages set by user that are not associated with an ID.
 *
 * E.g. (child nodes are optional)
 * <pre>
 * &lt;lift:snippet type="error_report"&gt;
 *   &lt;lift:error_msg&gt;Error!  The details are:&lt;/lift:error_msg&gt;
 *   &lt;lift:error_class&gt;errorBox&lt;/lift:error_class&gt;
 *   &lt;lift:warning_msg&gt;Whoops, I had a problem:&lt;/lift:warning_msg&gt;
 *   &lt;lift:warning_class&gt;warningBox&lt;/lift:warning_class&gt;
 *   &lt;lift:notice_msg&gt;Note:&lt;/lift:notice_msg&gt;
 *   &lt;lift:notice_class&gt;noticeBox&lt;/lift:notice_class&gt;
 * &lt;/lift:snippet&gt;
 * </pre>
 *
 */
object Msgs extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case _ => render
  }

  def render(styles: NodeSeq): NodeSeq = {
    val f = noIdMessages _
    val msgs = List((f(S.errors),
                     (styles \\ "error_msg"), S.??("msg.error"),
                     ((styles \\ "error_class") ++
                      (styles \\ "error_msg" \\ "@class")), 0),
                    (f(S.warnings),
                     (styles \\ "warning_msg"), S.??("msg.warning"),
                     ((styles \\ "warning_class")++
                      (styles \\ "warning_msg" \\ "@class")), 1),
                    (f(S.notices),
                     (styles \\ "notice_msg"), S.??("msg.notice"),
                     ((styles \\ "notice_class")) ++
                     (styles \\ "notice_msg" \\ "@class"), 2)).flatMap
    {
      case (msg, titleList, defaultTitle, styleList, ord) =>
        val title: String = titleList.toList. filter(_.prefix == "lift").
        map(_.text.trim).filter(_.length > 0) headOr defaultTitle
        val styles = styleList.toList.map(_.text.trim)
        if (!styles.isEmpty) {
          ord match {
            case 0 => MsgsErrorMeta(Full(AjaxMessageMeta(Full(title),
                                                         Full(styles.mkString(" ")))))
            case 1 => MsgsWarningMeta(Full(AjaxMessageMeta(Full(title),
                                                           Full(styles.mkString(" ")))))
            case 2 => MsgsNoticeMeta(Full(AjaxMessageMeta(Full(title),
                                                          Full(styles.mkString(" ")))))
          }
        }
        msg.toList.map(e => (<li>{e}</li>) ) match {
          case Nil => Nil
          case msgList => val ret = (<div>{title}<ul>{msgList}</ul></div>)
            styles.foldLeft(ret)((xml, style) => xml % new UnprefixedAttribute("class", Text(style), Null))
        }
    }
    <div>{msgs}</div> % ("id" -> LiftRules.noticesContainerId)
  }
}

object MsgsNoticeMeta extends SessionVar[Box[AjaxMessageMeta]](Empty)
object MsgsWarningMeta extends SessionVar[Box[AjaxMessageMeta]](Empty)
object MsgsErrorMeta extends SessionVar[Box[AjaxMessageMeta]](Empty)

case class AjaxMessageMeta(title: Box[String], cssClass: Box[String])
