package experiments.better_files

import better.files.File._

object ZipAPI extends App {

  ReadWrite.main(args)

  val betterFiles = currentWorkingDirectory / "src" / "main" / "resources" / "better-files"
  val zipFile = betterFiles.parent / "better-files.zip"
  val unzipFolder = betterFiles.parent / "better-files-unzipped"

  if (zipFile.exists) zipFile.delete()
  unzipFolder.createIfNotExists(asDirectory = true)

  betterFiles.zipTo(zipFile)
  zipFile.unzipTo(unzipFolder)

  val equals = betterFiles === unzipFolder
  if (equals) println("zip -> unzip equals original")
  else println("zip -> unzip did not equal original")
}
