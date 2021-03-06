import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

  val appName = "yabe"
  val appVersion = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    "com.octo.captcha" % "jcaptcha" % "1.0")

  val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings( // Add your own project settings here      
  )

}
