package controllers

import play.api._
import play.api.mvc._
import models._

import play.api.data._
import play.api.data.Forms._
import anorm.NotAssigned

import java.util.Date

object Application extends Controller {
  val commentForm = Form(
    tuple(
      "author" -> text,
      "content" -> text
    )
  )

  def index = Action {
    val allPosts = Post.allWithAuthorAndComments
    Ok(views.html.index(
      allPosts.headOption,
      allPosts.drop(1)))
  }

  def show(id: Long) = Action {
    Post.byIdWithAuthorAndComments(id).map {
      post =>
        Ok(views.html.show(post, post._1.prevNext))
    } getOrElse {
      NotFound("No such Post")
    }
  }

 
  def postComment(postId: Long) = Action{ implicit request =>
    val (author, content) = commentForm.bindFromRequest.get
    Comment.create(Comment(NotAssigned, author, content, new Date, postId))
    Post.byIdWithAuthorAndComments(postId).map {
      post =>
        Ok(views.html.show(post, post._1.prevNext))
    } getOrElse {
      NotFound("No such Post")
    }
  }
}