/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package frontend

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}

import scala.util.Try

/** A Frontend factory which takes as input: context + compiler arguments + callback */
trait FrontendFactory {
  def apply(ctx: inox.Context, compilerArgs: Seq[String], callback: MasterCallBack): Frontend

  protected val extraCompilerArguments: Seq[String] = Nil
  protected val libraryPaths: Seq[String]
  private lazy val cl = getClass.getClassLoader

  /** Paths to the library files used by this frontend. */
  final lazy val libraryFiles: Seq[String] = libraryPaths map (_.replace("\\", "/")) map cl.getResource map { url =>
    // There are two run modes: either the library is not packaged in a jar, and therefore
    // directly available as is from the disk, or it is embedded in stainless' jar file, in
    // which case we need to extract the files to a temporary location in order to let the
    // underlying compiler read them.

    val file = Try(Paths.get(url.toURI()).toFile).toOption
    val uri = url.getFile

    //val file = new File(path)
    if (file.exists(f => f.exists && f.isFile)) file.get.getAbsolutePath
    else {
      // JAR URL syntax: jar:<url>!/{filepath}, Expected path syntax: file:/path/a.jar!/{filepath}
      assert((uri startsWith "file:") || (uri startsWith "jar:file:"))
      val Array(_, filepath) = uri split "!/"
      val filename = filepath.replaceAllLiterally("/", "_")
      val splitPos = filename lastIndexOf '.'
      val (prefix, suffix) = filename splitAt splitPos
      val tmpFilePath = Files.createTempFile(prefix, suffix)
      val stream = url.openStream()
      Files.copy(stream, tmpFilePath, StandardCopyOption.REPLACE_EXISTING)
      stream.close()
      tmpFilePath.toFile.deleteOnExit()
      tmpFilePath.toString
    }
  }

  /** All the arguments for the underlying compiler. */
  protected def allCompilerArguments(compilerArgs: Seq[String]): Seq[String] =
    extraCompilerArguments ++ libraryFiles ++ compilerArgs
}

