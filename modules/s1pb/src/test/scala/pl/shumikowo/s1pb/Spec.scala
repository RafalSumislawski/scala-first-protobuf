package pl.shumikowo.s1pb

import java.nio.file.{Files, Paths}
import java.util

import com.typesafe.scalalogging.StrictLogging
import org.apache.commons
import org.specs2.mutable.Specification
import Spec._

trait Spec extends Specification with StrictLogging {
  def save(name: String, content: String): Unit = {
    val targetDir = cleanTargetDirectory
    val protoFilePath = targetDir.resolve(s"$name.proto")
    logger.debug(s"Proto file $protoFilePath")
    Files.write(protoFilePath, util.Arrays.asList(content))
    ()
  }
}

object Spec {
  private lazy val cleanTargetDirectory = {
    // this cleans the target directory once per test run
    val targetDir = Paths.get("modules/s1pb/target/protobuf")
    commons.io.FileUtils.deleteDirectory(targetDir.toFile)
    Files.createDirectories(targetDir)
  }
}
