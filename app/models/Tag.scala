package models
import anorm._
import anorm.SqlParser._
import java.util.Date

import play.api.db._
import play.api.Play.current

case class Tag(
  id: Pk[Long] = NotAssigned,
  name: String)

object Tag {
  val simple = {
    get[Pk[Long]]("id") ~ get[String]("name") map { case id ~ name => Tag(id, name) }
  }
  def apply(name: String) = new Tag(NotAssigned, name)

  def create(tag: Tag): Tag = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into tag(name) values ( {name} )
        """).on('name -> tag.name).executeInsert(scalar[Long] singleOpt)
    } match {
      case Some(id) => Tag(Id(id), tag.name)
      case None => tag
    }
  }

  def findOrCreateByName(name: String): Tag = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          select * from tag where name = {name}
        """)
        .on('name -> name)
        .as(Tag.simple singleOpt) getOrElse {
          Tag.create(Tag(name))
        }
    }
  }

  def byId(id: Long): Tag = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          select * from tag where id = {id}
        """)
        .on('id -> id)
        .as(Tag.simple single)
    }
  }
}

case class TagsForPosts(id: Pk[Long],
  tag_id: Long,
  post_id: Long)

object TagsForPosts {

  val simple = {
    get[Pk[Long]]("id") ~ get[Long]("tag_id") ~ get[Long]("post_id") map {
      case id ~ tag ~ post => TagsForPosts(id, tag, post)
    }

  }
  def apply(tag_id: Long, post_id: Long) =
    new TagsForPosts(NotAssigned, tag_id, post_id)

  def create(tfp: TagsForPosts): TagsForPosts = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into TagsForPosts(tag_id, post_id) values ( {tag_id} , {post_id} )
        """).on('tag_id -> tfp.tag_id, 'post_id -> tfp.post_id).executeUpdate()
    }
    tfp
  }

  def link(tag_id: Long, post_id: Long): Option[Long] = {

    DB.withConnection { implicit connection =>
      val maybeExistingTagAndPost = SQL(
        """
          select * from TagsForPosts where tag_id = {tag_id} and post_id = {post_id}
        """)
        .on('tag_id -> tag_id, 'post_id -> post_id)
        .as((TagsForPosts.simple *)).headOption

      // TODO: tag_id and/or post_id may not be exist (fk error?)!
      maybeExistingTagAndPost match {
        case Some(_) => maybeExistingTagAndPost.get.id.toOption
        case None => TagsForPosts.create(TagsForPosts(tag_id, post_id)).id.toOption
      }
    }
  }

  def getCloud: List[(String, Long)] = {
    DB.withConnection { implicit connection =>
      SQL(""" 
            SELECT t.name, count(p.id) as totalPosts 
            FROM Post p
            JOIN TagsForPosts tfp on p.id=tfp.post_id
            JOIN Tag t ON tfp.tag_id=t.id
            GROUP BY t.name ORDER BY t.name  
            """).as(get[String]("name") ~ get[Long]("totalPosts") map {
        case name ~ count => (name, count)
      }*)
    }
  }
}