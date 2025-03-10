import sbt._
import sbt.Keys._
import com.typesafe.sbt.packager.graalvmnativeimage.GraalVMNativeImagePlugin

object Config {
  val baseConfig = Seq(
    scalaVersion := "3.6.3",
    organization := "ScalaForge",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test,
    scalacOptions ++= Seq(
      "-Wunused:all",
      "-Wall"
    )
  )

  val ccConfig = Seq(
    scalacOptions ++= Seq(
      //"-Ycc-debug"
    )
  )

  val coreConfig = baseConfig ++ ccConfig ++ Seq(
    name := "traveler-core"
  )

  val nativeDemo = baseConfig ++ Seq(
    name := "traveler-native-demo"
  )
}
