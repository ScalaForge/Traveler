import sbt._
import sbt.Keys._

object Config {
  val baseConfig = Seq(
    scalaVersion := "3.6.2-RC3",
    organization := "ScalaForge",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test,
    scalacOptions ++= Seq(
      "-Wunused", "all",
      "-Wall"
    )
  )

  val coreConfig = baseConfig ++ Seq(
    name := "traveler-core"
  )
}
