package com.foo.jpaweb.snippet

import scala.xml.{NodeSeq,Text}

import net.liftweb.http.{RequestVar,S,SHtml}
import net.liftweb.util.{Helpers,Log}
import S._
import Helpers._

import com.foo.jpaweb.model._
import Model._

class AuthorOps {
  def list (xhtml : NodeSeq) : NodeSeq = {
    val authors = Model.createNamedQuery[Author]("findAllAuthors").getResultList()

    authors.flatMap(author =>
      bind("author", xhtml,
	   "name" -> Text(author.name),
	   "count" -> SHtml.link("/books/search", {() =>
	     BookOps.resultVar(Model.createNamedQuery[Book]("findBooksByAuthor", "id" ->author.id).getResultList().toList)
	     }, Text(author.books.size().toString)),
	   "edit" -> SHtml.link("add", () => authorVar(author), Text(?("Edit")))))
  }

  // Set up a requestVar to track the author object for edits and adds
  object authorVar extends RequestVar(new Author())
  def author = authorVar.is

  def add (xhtml : NodeSeq) : NodeSeq = {
    def doAdd = Model.wrapEM({
      if (author.name.length == 0) {
	error("emptyAuthor", "The author's name cannot be blank")
      } else {
	Model.flush()
	redirectTo("list")
      }
    }, {
      case cv : ConstraintViolation => S.error("An Author with that name already exists")
      case _ => S.error("Internal error")
    })

    // Hold a val here so that the "id" closure re-injects it when we re-enter this method
    val heldAuthor = author

    bind("author", xhtml,
	 "id" -> SHtml.hidden({authorVar(Model.merge(heldAuthor))}),
	 "name" -> SHtml.text(author.name, author.name = _),
	 "submit" -> SHtml.submit(?("Save"), doAdd _))
  }
}
