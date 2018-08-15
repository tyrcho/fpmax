name := "FP to the Max"
version := "0.1-SNAPSHOT"
scalaVersion := "2.12.6"
libraryDependencies ++= Seq(monix, scalaTest)
scalacOptions += "-Ypartial-unification"

lazy val scalaTest = "org.scalatest" %% "scalatest"  % "3.0.4" % Test
lazy val monix     = "io.monix"      %% "monix-eval" % "3.0.0-RC1"
