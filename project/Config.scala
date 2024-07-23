import sbt._
import sbt.Keys._

object Config {
  val baseConfig = Seq(
    scalaVersion := "3.5.0-RC4",
    organization := "ScalaForge",
  )

  val coreConfig = baseConfig ++ Seq(
    name := "traveler-core",
  )
}