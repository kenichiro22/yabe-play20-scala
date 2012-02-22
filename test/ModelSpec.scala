import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._
import anorm._
import java.util.Date
import org.yaml.snakeyaml.Yaml

class ModelSpec extends Specification {

  import models._

  "User model" should {

    "be created and retrieved by email and password" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        val u = new User(NotAssigned, "bob@gmail.com", "secret", "Bob", false)
        u.id.isDefined must beFalse
        User.create(u);

        // id must be defined
        User.authenticate("bob@gmail.com", "secret").get.id.isDefined must beTrue

        User.authenticate("bob@gmail.com", "secret") should not be (None)
        User.authenticate("bob@gmail.com", "badpassword") should be(None)
        User.authenticate("tom@gmail.com", "secret") should be(None)
      }
    }
  }

  "Post model" should {
    "be created" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob", false))
        Post.create(Post(NotAssigned, "My first post", "Hello!", new Date, 1))

        Post.count() must beEqualTo(1)  
      }
    }
    "be retrieved with author" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob", false))
        Post.create(Post(NotAssigned, "My 1st post", "Hello world", new Date, 1))

        val posts = Post.allWithAuthor

        posts.length should beEqualTo(1)

        val (post, author) = posts.head
        post.title must beEqualTo("My 1st post")
        author.fullname must beEqualTo("Bob")
      }
    }

    "support comments" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob", false))
        Post.create(Post(Id(1), "My first post", "Hello world", new Date, 1))
        Comment.create(Comment(NotAssigned, "Jeff", "Nice post", new Date, 1))
        Comment.create(Comment(NotAssigned, "Tom", "I knew that !", new Date, 1))

        User.count() must beEqualTo(1)
        Post.count() must beEqualTo(1)
        Comment.count() must beEqualTo(2)

        val list = Post.allWithAuthorAndComments

        list.size must beEqualTo(1)
        list(0)._1.title must beEqualTo("My first post")
        list(0)._2.email must beEqualTo("bob@gmail.com")

        val comments = list(0)._3
        comments.size must beEqualTo(2)
        comments(0).author must beEqualTo("Jeff")
        comments(1).author must beEqualTo("Tom")

        val Some((post, author, comments2)) = Post.byIdWithAuthorAndComments(1)

        post.title must beEqualTo("My first post")
        author.fullname must beEqualTo("Bob")
        comments.length must beEqualTo(2)
        comments(0).author must beEqualTo("Jeff")
        comments(1).author must beEqualTo("Tom")
      }
    }
  }
}