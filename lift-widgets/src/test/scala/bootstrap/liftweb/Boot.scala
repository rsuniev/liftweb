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
package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.widgets.calendars._
import _root_.net.liftweb.widgets.tree.TreeView
import _root_.net.liftweb.widgets.menu.MenuWidget
import _root_.net.liftweb.widgets.sparklines.Sparklines
import net.liftweb.widgets.sparklines.Sparklines
import net.liftweb.widgets.tablesorter.TableSorter

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("webapptest")

    // Build SiteMap

    val entries = Menu(Loc("Home", List("index"), "Home"),
           Menu(Loc("submenu1", List("submenu1"), "submenu1")),
           Menu(Loc("submenu2", List("submenu2"), "submenu2")),
           Menu(Loc("submenu3", List("submenu3"), "submenu3"),
                Menu(Loc("anothermenu1", List("anothermenu1"), "anothermenu1")),
                Menu(Loc("anothermenu2", List("anothermenu2"), "anothermenu2")))) ::
      Menu(Loc("calmonth", List("calmonth"), "CalendarMonthView")) ::
      Menu(Loc("calweek", List("calweek"), "CalendarWeekView")) ::
      Menu(Loc("calday", List("calday"), "CalendarDayView")) ::
      Menu(Loc("rssfeed", List("rssfeed"), "RSSFeed")) ::
      Menu(Loc("gravatear", List("gravatar"), "Gravatar")) ::
      Menu(Loc("tree", List("tree"), "Tree")) ::
      Menu(Loc("sparklines", List("sparklines"), "SparkLines")) ::
      Nil

    LiftRules.setSiteMap(SiteMap(entries:_*))

    CalendarMonthView init;
    CalendarWeekView init;
    CalendarDayView init;
    TreeView init;
    Sparklines init;
    TableSorter init;
    MenuWidget init;

  }
}

