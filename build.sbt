name := "FP to the Max"
version := "0.1-SNAPSHOT"
scalaVersion := "2.12.6"
libraryDependencies += scalaTest
libraryDependencies ++= cats
scalacOptions += "-Ypartial-unification"

lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.4" % Test
lazy val cats      = Seq("org.typelevel" %% "cats-core" % "1.2.0", "org.typelevel" %% "cats-effect" % "1.0.0-RC2")
