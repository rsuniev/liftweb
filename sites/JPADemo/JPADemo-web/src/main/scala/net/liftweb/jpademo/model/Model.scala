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
package net.liftweb.jpademo.model

import javax.persistence._

import java.sql.SQLException
import javax.naming.InitialContext
import javax.transaction.{Status,UserTransaction}

import org.hibernate.HibernateException
import org.hibernate.exception.ConstraintViolationException

import net.liftweb.util.Log

class ConstraintViolation(message : String) extends Exception(message)

object Model extends ScalaEntityManager("jpaweb") {
  lazy val ctxt = new InitialContext()
  //lazy val factory = ctxt.lookup("java:comp/env/" + persistanceName).asInstanceOf[EntityManagerFactory]

  def tx = ctxt.lookup("java:comp/UserTransaction").asInstanceOf[UserTransaction]

  val txStatus = Map(Status.STATUS_ACTIVE -> "ACTIVE",
		     Status.STATUS_COMMITTED -> "COMMITTED",
		     Status.STATUS_COMMITTING -> "COMMITTING",
		     Status.STATUS_MARKED_ROLLBACK -> "MARKED_ROLLBACK",
		     Status.STATUS_NO_TRANSACTION -> "NO_TRANSACTION",
		     Status.STATUS_PREPARED -> "PREPARED",
		     Status.STATUS_PREPARING -> "PREPARING",
		     Status.STATUS_ROLLING_BACK -> "ROLLING_BACK",
		     Status.STATUS_ROLLEDBACK -> "ROLLEDBACK",
		     Status.STATUS_UNKNOWN -> "UNKNOWN")

  override def openEM () = {
    //val tx = txObj
    Log.debug("Got transaction: " + tx)
    tx.begin()

    val em = ctxt.lookup("java:comp/env/persistence/" + persistanceName).asInstanceOf[EntityManager]

    Log.debug("TX status = " + txStatus(tx.getStatus()))

    em
  }

  override def closeEM (em : EntityManager) = {
    em.close()

    /* We only want to commit if we haven't already thrown an exception (due to a constraint violation, etc)
     */
    try {
      if (tx.getStatus() == Status.STATUS_MARKED_ROLLBACK) {
	tx.rollback()
      } else if (tx.getStatus() == Status.STATUS_ACTIVE) {
	tx.commit()
	Log.debug("Committed TX")
      } else {
	Log.debug("TX status = " + txStatus(tx.getStatus()))
      }
    }
  }

  /**
   This method allows me to clean up my code a bit and only handle JPA-related exceptions.
   An example usage would be:

   def addFood(newFood : Food) =
     wrapEM({
       Model.persist(newFood)
       S.redirectTo("/food/list")
     }, {
       case cve : ConstraintViolationException => S.error("That food already exists!")
       case _ => S.error("Internal error adding food")
     })

   Note that if I used normal try/catch then the wildcard match would trap the RedirectException
   thrown by S.redirectTo.
  */
  def wrapEM(f : => Unit) : Unit = wrapEM(f, { case _ => /* nop */ })
  def wrapEM[A](f : => A, handler : PartialFunction[Throwable, A]) : A = {
    try {
      try {
	val ret : A = f
	ret
      } catch {
	case he : HibernateException => {
	  Log.error("Hibernate error: " + he.getMessage)
	  Log.debug(he)
	  handler(he)
	}
	case pe : PersistenceException => {
	  Log.error("EM Error: " + pe.getMessage)
	  Log.debug(pe)
	  pe.getCause() match {
	    case cve : ConstraintViolationException => handler(new ConstraintViolation(cve.getMessage()))
	    case _ => handler(pe)
	  }
	}
	case sqle : java.sql.SQLException => {
	  Log.error("SQL Exception: " + sqle.getMessage)
	  Log.debug(sqle)
	  handler(sqle)
	}
      }
    } catch {
      // Special case. Usually we want to know why it failed to commit, not just that it failed
      case re : RollbackException => {
	val (cause,message) = if (re.getCause() == null) {
	  (re,"No cause")
	} else {
	  (re.getCause(), re.getCause().getMessage())
	}
	Log.error("EM Commit error", re)
	handler(cause)
      }
    }
  }
}

