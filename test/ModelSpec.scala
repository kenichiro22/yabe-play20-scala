import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._
import anorm._
import java.util.Date
import org.yaml.snakeyaml.Yaml
import org.joda.time.DateTime
import org.specs2.matcher.ShouldExpectable

class ModelSpec extends Specification {

  import models._

  "User model" should {

    "be created and retrieved by email and password" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        User.create(User("bob@gmail.com", "secret", "Bob", false));

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
        Post.create(Post("My first post", "Hello!", new Date, 1))

        Post.count() must beEqualTo(1)
      }
    }
    "be retrieved with author" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob", false))
        Post.create(Post("My 1st post", "Hello world", new Date, 1))

        val posts = Post.allWithAuthor

        posts.length should beEqualTo(1)

        val (post, author) = posts.head
        post.title must beEqualTo("My 1st post")
        author.fullname must beEqualTo("Bob")
      }
    }

    "support prev and next" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob", false))
        val posts = Seq(
          Post.create(Post(Id(1), "My first post", "Hello world", new DateTime("2012-01-01").toDate, 1)),
          Post.create(Post(Id(2), "My second post", "Hello world", new DateTime("2012-01-02").toDate, 1)),
          Post.create(Post(Id(3), "My third post", "Hello world", new DateTime("2012-01-03").toDate, 1)))

        // No prev
        val post1prevNext = posts(0).prevNext;
        post1prevNext._1.get.id.get must beEqualTo(2)
        post1prevNext._2 must beNone

        // Prev Next
        val post2prevNext = posts(1).prevNext;
        post2prevNext._1.get.id.get must beEqualTo(3)
        post2prevNext._2.get.id.get must beEqualTo(1)

        // No Next
        val post3prevNext = posts(2).prevNext;
        post3prevNext._1 must beNone
        post3prevNext._2.get.id.get must beEqualTo(2)

      }
    }

    "support comments" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create(User(Id(1), "bob@gmail.com", "secret", "Bob", false))

        Post.create(Post(Id(1), "My first post", "Hello world", new DateTime("2012-01-01").toDate, 1))
        Post.create(Post(Id(2), "Second post", "No comment", new DateTime("2012-01-02").toDate, 1))
        Comment.create(Comment(NotAssigned, "Jeff", "Nice post", new Date, 1))
        Comment.create(Comment(NotAssigned, "Tom", "I knew that !", new Date, 1))

        User.count() must beEqualTo(1)
        Post.count() must beEqualTo(2)
        Comment.count() must beEqualTo(2)

        val list = Post.allWithAuthorAndComments
        list.size must beEqualTo(2)

        list(1)._1.title must beEqualTo("My first post")
        list(1)._2.email must beEqualTo("bob@gmail.com")

        val comments = list(1)._3
        comments.size must beEqualTo(2)
        comments(0).author must beEqualTo("Jeff")
        comments(1).author must beEqualTo("Tom")

        list(0)._3 must beEmpty

        val Some((post, author, comments2)) = Post.byIdWithAuthorAndComments(1)

        post.title must beEqualTo("My first post")
        author.fullname must beEqualTo("Bob")
        comments.length must beEqualTo(2)
        comments(0).author must beEqualTo("Jeff")
        comments(1).author must beEqualTo("Tom")
      }
    }

    "support Tags" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        User.create(User(Id(1), "bob@gmail.com", "secret1", "Bob Morane", false))
        val postJava = Post.create(Post(Id(1), "My first post", "Java and Scala: yes it rocks!", new Date, 1))
        val javaTag = Tag.create(Tag(Id(1), "Java"))
        val scalaTag = Tag.create(Tag(Id(2), "Scala"))
        
        Post.findTaggedWith("Java").length should beEqualTo(0)
        Post.findTaggedWith("Scala").length should beEqualTo(0)

        postJava.tagItWith("Java")
        postJava.tagItWith("Scala")
        Post.findTaggedWith("Java").length should beEqualTo(1)
        Post.findTaggedWith("Scala").length should beEqualTo(1)

        postJava.tagItWith("A new Tag that does not already exist")

        // Should reuse existing TagsForPosts
        postJava.tagItWith("Scala")
        Post.findTaggedWith("Java").length should beEqualTo(1)
        Post.findTaggedWith("Scala").length should beEqualTo(1)

        // Another post
        val postScala = Post.create(Post(Id(2), "A scala post", "Scala only", new Date, 1))
        postScala.tagItWith("Scala")
        Post.findTaggedWith("Scala").length should beEqualTo(2)

        // Let's see what happens when we delete a Post
        //        Post.delete("where id={pid}").on("pid" -> postJava.id()).executeUpdate()
        //        Post.findTaggedWith("Scala").length should beEqualTo(1)
      }
    }
  }

  "Tag model" should {
    "support Tag cloud" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create(User(Id(1), "bob@gmail.com", "secret1", "Bob Morane", false))

        val postJava = Post.create(Post(Id(1), "My first post", "Java 7 is out!", new Date, 1))
        postJava.tagItWith("Java")
        postJava.tagItWith("JEE")

        val postScalaJava = Post.create(Post(Id(2), "Another post", "Java and Scala: yes it rocks!", new Date, 1))
        postScalaJava.tagItWith("Java")
        postScalaJava.tagItWith("Scala")

        val cloud: List[(String, Long)] = TagsForPosts.getCloud

        cloud.length should beEqualTo(3)

        cloud.map { tagAndTotal =>
          tagAndTotal match {
            case ("Java", cpt) => cpt should beEqualTo(2)
            case ("Scala", cpt) => cpt should beEqualTo(1)
            case ("JEE", cpt) => cpt should beEqualTo(1)
            case (_, cpt) => cpt should beEqualTo(0)
          }
        }
      }
    }
  }
}