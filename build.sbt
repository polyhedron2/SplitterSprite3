val gameVersion = "1.0.0"
name := "SplitterSprite3"
version := gameVersion
assemblyJarName := s"../../game/ver${gameVersion}/game.jar"
mainClass := Some("jp.gr.java_conf.polyhedron.splittersprite3.Spore")
scalacOptions ++= Seq("-deprecation")
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"
libraryDependencies += "org.mockito" % "mockito-core" % "2.13.0" % "test"
libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "2.0.1")
