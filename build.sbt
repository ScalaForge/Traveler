organization := "ScalaForge"
name := "Traveler"

lazy val root =
  project.in(file(".")).aggregate(core, `core-bench`, `sbt-plugin`)

lazy val core = project.settings(
  Config.coreConfig
)

lazy val `core-bench` = project
  .settings(
    Config.baseConfig
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core)

lazy val `sbt-plugin` = project
  .enablePlugins(SbtPlugin)
  .settings(
    Config.pluginConfig
  )
