package models
import anorm._
import anorm.SqlParser._
import java.util.Date

import play.api.db._
import play.api.Play.current

case class Comment(
  id: Pk[Long],
  author: String, content: String, postedAt: Date, postId: Long)

object Comment {
  /* Parsers */

  /**
   * Parse a Post from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("comment.id") ~
      get[String]("comment.author") ~
      get[String]("comment.content") ~
      get[Date]("comment.postedAt") ~
      get[Long]("comment.post_id") map {
        case id ~ author ~ content ~ postedAt ~ postId => Comment(id, author, content, postedAt, postId)
      }
  }

  def create(comment: Comment): Comment = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into comment(author, content, postedAt, post_id) values (
            {author}, {content}, {postedAt}, {post_id}
          )
        """).on(
          'author -> comment.author,
          'content -> comment.content,
          'postedAt -> comment.postedAt,
          'post_id -> comment.postId).executeUpdate()
      comment
    }
  }

  def count() = {
    DB.withConnection { implicit connection =>
      SQL("select count(*) from Comment").as(scalar[Long].single)
    }
  }
}