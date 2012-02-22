package controllers

import play.api._
import play.api.mvc._
import models.Post

object Application extends Controller {
  
  def index = Action {
    
    val allPosts = Post.allWithAuthorAndComments
    Ok(views.html.index(
        allPosts.headOption, 
        allPosts.drop(1)
    ))
  }
  
}