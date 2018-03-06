val gameVersion = "1.0.0"
name := "SplitterSprite3"
version := gameVersion
assemblyJarName := s"../../game/ver${gameVersion}/game.jar"
mainClass := Some("jp.gr.java_conf.polyhedron.splittersprite3.Spore")
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.mockito" % "mockito-core" % "2.13.0" % "test"
