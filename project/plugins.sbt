// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Maven repo
resolvers += "Maven central repository" at "http://repo1.maven.org/maven2/"

resolvers += "jahia org repository" at "http://maven.jahia.org/maven2/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.0-RC3")

// libraryDependencies += "com.octo.captcha" % "jcaptcha" % "1.0"