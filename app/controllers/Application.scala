package controllers

import play.api._
import play.api.mvc._
import models._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import anorm.NotAssigned
import java.util.Date
import com.octo.captcha.service.image.DefaultManageableImageCaptchaService
import com.octo.captcha.service.image.ImageCaptchaService
import javax.imageio.ImageIO
import java.io.ByteArrayOutputStream
import java.util.Locale

object Application extends Controller {
  val commentForm = Form(
    tuple(
      "author" -> nonEmptyText,
      "content" -> nonEmptyText))

  def index = Action {
    val allPosts = Post.allWithAuthorAndComments
    Ok(views.html.index(
      allPosts.headOption,
      allPosts.drop(1)))
  }

  def show(id: Long) = Action {implicit request =>

    Post.byIdWithAuthorAndComments(id).map {
      post =>
        Ok(views.html.show(post, post._1.prevNext, commentForm))
    } getOrElse {
      NotFound("No such Post")
    }
  }

  def postComment(postId: Long) = Action { implicit request =>
    commentForm.bindFromRequest.fold(
      formWithErrors => {
        val post = Post.byIdWithAuthorAndComments(postId).get
        BadRequest(views.html.show(post, post._1.prevNext, formWithErrors))
      },
      comment => {
        Comment.create(Comment(NotAssigned, comment._1, comment._2, new Date, postId))
        Redirect(routes.Application.show(postId)).flashing("success" -> "Thanks for posting %s".format(comment._1))
      })
  }
  
  val captchaService :ImageCaptchaService = new DefaultManageableImageCaptchaService
  
  def captcha = Action{ implicit request => 
    // http://stackoverflow.com/questions/8305853/how-to-render-a-binary-with-play-2-0
    // http://d.hatena.ne.jp/kaiseh/20090502/1241286415
    val baos = new ByteArrayOutputStream
    ImageIO.write(captchaService.getImageChallengeForID("", Locale.getDefault()), "jpg", baos);
    Ok(baos.toByteArray()).as("image/jpeg")
  }
  
  
}