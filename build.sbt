organization := "ScalaForge"
name := "Traveler"

lazy val core = project.settings(
  Config.coreConfig
)

// lazy val `core-bench` = project
//   .settings(
//     Config.baseConfig
//   )
//   .enablePlugins(JmhPlugin)
//   .dependsOn(core)

lazy val root = project.in(file(".")).aggregate(core)
