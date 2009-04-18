/*
 * Copyright 2008 WorldWide Conferencing, LLC
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
package net.liftweb.textile

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

import _root_.scala.xml._

class TextileSpecTest extends Runner(TextileSpec) with JUnit with Console
object TextileSpec extends Specification {
  import TextileParser._

  "A Textile Parse" can {
    "Be a single line of text" in {
      toHtml("Hello World") must ==/(<p>Hello World</p>)
    }

    "Make things bold" in {
      toHtml("**Hello World**") must ==/(<p><b>Hello World</b></p>)
    }

    "Make other things bold" in {
      toHtml("Dude this is **Hello World** kind of stuff") must ==/(<p>Dude this is <b>Hello World</b> kind of stuff</p>)
    }

    "I am <em>very</em> serious" in {
      toHtml("I am <em>very</em> serious") must ==/(<p>I am <em>very</em> serious</p>)
    }

    "Observe -- very nice!" in {
      toHtml("Observe -- very nice!") must ==/(<p>Observe &#8212; very nice!</p>)
    }

    "Observe - tiny and brief." in {
      toHtml("Observe - tiny and brief.") must ==/(<p>Observe &#8211; tiny and brief.</p>)
    }

    "\"Observe!\"" in {
      val ret = toHtml("\"Observe!\"")

      ret must ==/(<p>&#8220;Observe!&#8221;</p>)
    }

    "A simple example." in {
      toHtml("A simple example.") must ==/(<p>A simple example.</p>)
    }

    "Multi-line with - signs" in {
      val res = toHtml(
        """
A simple example.
A hi-tech example.
A hi-tech lo-tech example.
A -deleted- example.
A -deleted hi-tech- example.
A regular example.
""")
      
      res must ==/(<p>A simple example.<br />
          A hi-tech example. <br />
          A hi-tech lo-tech example. <br />
          A <del>deleted</del> example.<br />
          A <del>deleted hi-tech</del> example. <br />
          A regular example.</p>)
    }

    "a link http://yahoo.com inside" in {
      toHtml("a link http://yahoo.com inside") must ==/(<p>a link <a href="http://yahoo.com">http://yahoo.com</a> inside</p>)
    }

    "h3" in {
      toHtml("h3. Dogs eat dogfood") must ==/(<h3>Dogs eat dogfood</h3>)
    }

    "h3 with whitespace" in {
      toHtml("   h3. Dogs eat dogfood") must ==/(<h3>Dogs eat dogfood</h3>)
    }

    "Single line delete " in {
      toHtml("This contains -deleted stuff-") must ==/(<p>This contains <del>deleted stuff</del></p>)
    }

    "3 bullets" in {
      val it = toHtml(
        """
* Hello
* Dude
* Dog
""")

      it must ==/(
        <ul><li> Hello</li>
        <li> Dude</li>
        <li> Dog</li>
        </ul>
      )
    }

    "3 bullets strong" in {
      val it = toHtml(
        """
* *Hello* moo
* Dude
* Dog
""")

      it must ==/(
        <ul><li> <strong>Hello</strong> moo</li>
        <li> Dude</li>
        <li> Dog</li>
        </ul>
      )
    }

    "3 bullets not strong" in {
      val it = toHtml(
        """
* *Hello moo
* Dude
* Dog
""")

      it must ==/(
        <ul><li> *Hello moo</li>
        <li> Dude</li>
        <li> Dog</li>
        </ul>
      )
    }

    "a link http://yahoo.com not inside" in {
      toHtml("a link http://yahoo.com not inside", true).toString.trim must_== "<p>a link http://yahoo.com not inside</p>"
    }

  }
  """VF-DE:
        IFC.ksh: 04/10/08 03:26:52: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/10/08 15:34:30: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
VF-ES:
        IFC.ksh: 04/11/08 03:11:48: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 04:48:24: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
VF-GR:
        IFC.ksh: 04/11/08 03:11:59: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 03:46:47: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
VF-IE:
        IFC.ksh: 04/11/08 03:15:07: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 05:36:57: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
VF-IT:
        IFC.ksh: 04/11/08 03:05:07: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 05:06:09: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
VF-NL:
        IFC.ksh: 04/11/08 03:55:05: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 04:28:57: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
VF-PT:
        IFC.ksh: 04/11/08 03:00:09: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 03:32:05: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
VF-UK:
        IFC.ksh: 04/11/08 03:38:41: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 05:26:49: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *

I saved the entry by pressing "Yep" button, but because the symbol "-" caused the subsequent letters to be crossed out I've edited the text like this:
DE:
        IFC.ksh: 04/10/08 03:26:52: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/10/08 15:34:30: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
ES:
        IFC.ksh: 04/11/08 03:11:48: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 04:48:24: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
GR:
        IFC.ksh: 04/11/08 03:11:59: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 03:46:47: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
IE:
        IFC.ksh: 04/11/08 03:15:07: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 05:36:57: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
IT:
        IFC.ksh: 04/11/08 03:05:07: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 05:06:09: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
NL:
        IFC.ksh: 04/11/08 03:55:05: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 04:28:57: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
PT:
        IFC.ksh: 04/11/08 03:00:09: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 03:32:05: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *
UK:
        IFC.ksh: 04/11/08 03:38:41: * * * * * * * * * * * * * * * start of IFC.ksh * * * * * * * * * * * * * * *
        IFC.ksh: 04/11/08 05:26:49: * * * * * * * * * * * * * * * end of IFC.ksh * * * * * * * * * * * * * * *"""

  """Hello:
* THis is a * on a line
* This is a *strong* line
* This is a **Bold** line
* This is a line with no markup
"""

}
