package traveler.sbt

import sbt._
import Keys._
import sbt.util.CacheStore
import sbt.util.CacheResult
import java.security.MessageDigest
import scala.sys.process._
import java.nio.file.Files
import sbt.util.Hit
import java.util.Properties
import traveler.sbt.TravelerPlugin.autoImport.baseTravelerPluginSettings

object TravelerPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val compileCPP = taskKey[Seq[File]]("OBJ generation")
    val includeFiles =
      settingKey[Seq[File]]("Files to include in the compilation process")
    val linkerLibs = settingKey[Seq[String]]("Libs to be included")
    val filesLocations = settingKey[Seq[File]]("Location of C helper files")
    val travelerResourceLocation = settingKey[String]("Shared file output")

    lazy val baseTravelerPluginSettings: Seq[Def.Setting[_]] = Seq(
      compileCPP / filesLocations := Seq(
        resourceDirectory.value / "traveler-native"
      ),
      compileCPP / travelerResourceLocation := s"traveler-helpers/${name.value}-helper.so",
      compileCPP / linkerLibs := Seq.empty,
      compileCPP / includeFiles := Seq.empty,
      compileCPP := {
        compile(
          (compileCPP / filesLocations).value,
          (compileCPP / includeFiles).value,
          (compileCPP / linkerLibs).value,
          resourceManaged.value / (compileCPP / travelerResourceLocation).value,
          streams.value
        )
      },
      resourceGenerators += compileCPP.taskValue
    )
  }

  override lazy val projectSettings =
    inConfig(Compile)(baseTravelerPluginSettings)

  import autoImport._

  val compilationCache = CacheStore(file("/tmp/test"))

  def compile(
      files: Seq[File],
      includes: Seq[File],
      linkerLibs: Seq[String],
      out: File,
      s: TaskStreams
  ): Seq[File] = {
    val compileableFiles = files.flatMap(traverseFiles).map(_.getAbsolutePath())
    val command = Seq("clang") ++
      includes
        .flatMap(f => Seq("-I", f.absolutePath)) ++
      linkerLibs.map(name => s"-l$name") ++
      compileableFiles ++
      Seq("-shared", "-fPIC", "-o", out.getAbsolutePath())
    s.log.info(command.mkString(" "))
    out.getParentFile().mkdirs()
    val result = command.!
    if (result == 0) {
      Seq(out)
    } else {
      sys.error("Failed.")
    }
  }

  def traverseFiles(file: File): Seq[File] =
    if(file.isDirectory()) {
      file.listFiles().toSeq.flatMap(traverseFiles)
    } else {
      Seq(file)
    }

  def compileCpp(files: Seq[File], s: TaskStreams): File = {
    import sbt.util.CacheImplicits._
    val cachedFn =
      util.Tracked.inputChanged(s.cacheStoreFactory.make("input_diff")) {
        (inChange, _: Seq[Byte]) =>
          util.Tracked.outputChanged(s.cacheStoreFactory.make("output_diff")) {
            (outChanged, _: Seq[Byte]) =>
              if (inChange || outChanged) {
                ???
              }
          }
      }
    util.Tracked.diffInputs(
      s.cacheStoreFactory.make("input_diff"),
      FileInfo.hash
    )(files.toSet) { (inDiff: ChangeReport[File]) =>
      if (inDiff.unmodified == files.toSet) {}
      ???
    }
  }

  case class CompilationCache(key: Seq[Byte], location: File)
}
