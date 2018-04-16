name := "splittersprite"
version := "3-alpha"
organization := "polyhedron.java_conf.gr.jp"
scalacOptions ++= Seq("-deprecation")
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"
libraryDependencies += "org.mockito" % "mockito-core" % "2.13.0" % "test"
libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "2.0.1")
