import sbt._
import sbt.Keys._

object Config {
  val baseConfig = Seq(
    scalaVersion := "3.5.0-RC4",
    organization := "ScalaForge",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test
  )

  val coreConfig = baseConfig ++ Seq(
    name := "traveler-core",
  )
}