import sbt._
import sbt.Keys._
import sbt.ScriptedPlugin.autoImport.{scriptedLaunchOpts, scriptedBufferLog}

object Config {
  val baseConfig = Seq(
    scalaVersion := "3.7.0-RC2",
    organization := "ScalaForge",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test,
    scalacOptions ++= Seq(
      "-Wunused",
      "all",
      "-Wall"
    )
  )

  val ccConfig = Seq(
    scalacOptions ++= Seq(
      // "-Ycc-debug"
    )
  )

  val coreConfig = baseConfig ++ ccConfig ++ Seq(
    name := "traveler-core",
    Test / scalacOptions += "-language:experimental.captureChecking"
  )

  val pluginConfig = Seq(
    name := "sbt-traveler",
    organization := "fr.hammons",
    pluginCrossBuild / sbtVersion := {
      scalaBinaryVersion.value match {
        case "2.12" => "1.10.10"
      }
    },
    scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx1024M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false
  )
}
