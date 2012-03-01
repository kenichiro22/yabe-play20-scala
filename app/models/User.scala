package models
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class User(id: Pk[Long] = NotAssigned,
  email: String, password: String, fullname: String, isAdmin: Boolean)

object User {
  def apply(email: String, password: String, fullname: String, isAdmin: Boolean) = new User(NotAssigned, email, password, fullname, isAdmin)

  // -- Parsers

  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("user.id") ~
      get[String]("user.email") ~
      get[String]("user.password") ~
      get[String]("user.fullname") ~
      get[Boolean]("user.isAdmin") map {
        case id ~ email ~ password ~ fullname ~ isAdmin => User(id, email, password, fullname, isAdmin)
      }
  }

  /**
   * Authenticate a User.
   */
  def authenticate(email: String, password: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
         select * from user where 
         email = {email} and password = {password}
        """).on(
          'email -> email,
          'password -> password).as(simple.singleOpt)
    }
  }

  /**
   * Create a User.
   */
  def create(user: User): User = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into user(email, password, fullname, isAdmin) values (
            {email}, {password}, {fullname}, {isAdmin}
          )
        """).on(
          'email -> user.email,
          'fullname -> user.fullname,
          'password -> user.password,
          'isAdmin -> user.isAdmin).executeUpdate()
      user
    }
  }

  def count() = {
    DB.withConnection { implicit connection =>
      SQL("select count(*) from User").as(scalar[Long].single)
    }
  }

}