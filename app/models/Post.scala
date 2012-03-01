package models

import java.util.Date
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import collection.immutable.{ Nil, :: }

case class Post(
  id: Pk[Long] = NotAssigned,
  title: String, content: String, postedAt: Date, authorId: Long) {

  def prevNext: (Option[Post], Option[Post]) = {
    DB.withConnection {
      implicit connection =>
        val result = SQL(
          """
              (
                  select p.*, 'next' as pos from post p
                  where postedAt < {date} order by postedAt desc limit 1
              )
                  union
              (
                  select p.*, 'prev' as pos from post p
                  where postedAt > {date} order by postedAt asc limit 1
              )

              order by postedAt desc

          """).on("date" -> postedAt).as(
            Post.withPrevNext *).partition(_._2 == "prev")

        (result._1 match {
          case List((post, "prev")) => Some(post)
          case _ => None
        },
          result._2 match {
            case List((post, "next")) => Some(post)
            case _ => None
          })
    }
  }

  def tagItWith(name: String) = {
    val tag = Tag.findOrCreateByName(name)
    TagsForPosts.link(tag.id.get, id.get)
  }

  // Returns the list of Tag for this Post 
  def getTags: List[String] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          SELECT 
    		  T.NAME 
          FROM TAG t 
    		  JOIN TagsForPosts tfp ON tfp.tag_id=t.id 
    		  JOIN Post p on p.id=tfp.post_id 
          WHERE 
    		  p.id={id}
        """).on('id -> id.get)
        .as(get[String]("tag.name") *)
    }
  }
}

object Post {

  // -- Parsers

  /**
   * Parse a Post from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("post.id") ~
      get[String]("post.title") ~
      get[String]("post.content") ~
      get[Date]("post.postedAt") ~
      get[Long]("post.author_id") map {
        case id ~ title ~ content ~ postedAt ~ authorId => Post(id, title, content, postedAt, authorId)
      }
  }

  /**
   * Parse a (Post,User) from a ResultSet
   */
  val withUser = Post.simple ~ User.simple map {
    case post ~ user => (post, user)
  }

  /**
   * Parse a (Post,User) from a ResultSet
   */
  val withPrevNext = {
    get[Pk[Long]]("id") ~ get[String]("title") ~ get[String]("content") ~ get[Date]("postedAt") ~ get[Long]("author_id") ~ get[String]("pos") map {
      case id ~ title ~ content ~ postedAt ~ authorId ~ pos => (Post(id, title, content, postedAt, authorId), pos)
    }
  }

  /**
   * Parse a (Post, User, List[Comment]) from a ResultSet
   */
  val withAuthorAndComments = Post.simple ~ User.simple ~ (Comment.simple ?) map {
    case post ~ user ~ comments => (post, user, comments)
  }

  /**
   * Create a Post.
   */
  def create(post: Post): Post = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            insert into post(title, content, postedAt, author_id) values (
              {title}, {content}, {postedAt}, {author_id}
            )
          """).on(
            'title -> post.title,
            'content -> post.content,
            'postedAt -> post.postedAt,
            'author_id -> post.authorId).executeUpdate()
        post
    }
  }

  def allWithAuthor: List[(Post, User)] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
              select p.*, u.* from Post p
              join User u on p.author_id = u.id
              order by p.postedAt desc
          """).as(withUser *)
    }
  }

  def allWithAuthorAndComments: List[(Post, User, List[Comment])] =
    DB.withConnection {
      implicit connection =>
        SQL(
          """
              select p.*, u.*, c.* from Post p
              join User u on p.author_id = u.id
              left join Comment c on c.post_id = p.id
              order by p.postedAt desc
          """).as(withAuthorAndComments *)
          .groupBy(row => (row._1, row._2))
          .mapValues(_.unzip3._3.map(_.orNull))
          .map(row => row._2 match {
            case List(null) => (row._1._1, row._1._2, List())
            case comments => (row._1._1, row._1._2, comments)
          }).toList
    }

  def byIdWithAuthorAndComments(id: Long): Option[(Post, User, List[Comment])] =
    DB.withConnection {
      implicit connection =>
        Some(
          SQL(
            """
                select * from Post p
                join User u on p.author_id = u.id
                left join Comment c on c.post_id = p.id
                where p.id = {id}
            """)
            .on("id" -> id)
            .as(withAuthorAndComments *)
            .groupBy(row => (row._1, row._2))
            .mapValues(_.unzip3._3.map(_.orNull))
            .map(row => row._2 match {
              case List(null) => (row._1._1, row._1._2, List())
              case comments => (row._1._1, row._1._2, comments)
            }).head)
    }

  def count() = {
    DB.withConnection {
      implicit connection =>
        SQL("select count(*) from Post").as(scalar[Long].single)
    }
  }

  def findTaggedWith(name: String): List[Post] =
    DB.withConnection { implicit connection =>
      SQL("""
            select p.* from Post p
            join TagsForPosts tfp on p.id=tfp.post_id
            join Tag t on tfp.tag_id=t.id
            where t.name={name}
        """).on('name -> name).as(Post.simple *)
    }
}