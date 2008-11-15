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
package net.liftweb.jpademo.util

import java.util.Hashtable
import javax.naming.{Context,Name}
import javax.naming.spi.ObjectFactory

import javax.persistence.Persistence


class EntityManagerObjectFactory(unitName : String) extends ObjectFactory {
  println("Firing up EM Object Factory for " + unitName)

  val emf = Persistence.createEntityManagerFactory(unitName)

  def getObjectInstance(obj : Object, name : Name, context : Context, env : Hashtable[_,_]) = {
    emf.createEntityManager()
  }
}
    
