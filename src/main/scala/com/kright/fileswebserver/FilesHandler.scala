package com.kright.fileswebserver

import com.kright.fileswebserver.MyHttp.reply
import com.sun.net.httpserver.{HttpExchange, HttpHandler}

import java.io.{File, FileInputStream}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

class FilesHandler(val browserPath: Path,
                   private val fsPathProvider: FSPathProvider,
                   private val allowFileUploading: Boolean) extends HttpHandler:

  val browserRootPath: String = 
    "/" + browserPath.toString

  private def toValidFile(pathString: String): Option[File] =
    if (!pathString.startsWith("/")) return None
    val path = Path.of(pathString.substring(1))
    if (path.isAbsolute) return None

    val relativePath = browserPath.relativize(path)

    for {
      dirPath <- fsPathProvider.getAbsolutePath
      absolutePath = dirPath.resolve(relativePath).toAbsolutePath
      if absolutePath.startsWith(dirPath)
      file = absolutePath.toFile
      if file.exists()
    } yield file

  private def toBrowserPath(file: File): Option[String] =
    val absPath = file.toPath.toAbsolutePath

    for {
      dirPath <- fsPathProvider.getAbsolutePath
      if absPath.startsWith(dirPath)
      relativePath = dirPath.relativize(absPath)
    } yield s"/${browserPath.resolve(relativePath).toString}"

  private def encodePathAsLink(path: String): String =
    // it's better to use UrlEscapers.urlFragmentEscaper().escape(inputString);
    path.split("/").map(
      URLEncoder.encode(_, StandardCharsets.UTF_8).replace("+", "%20")
    ).mkString("/")

  override def handle(httpExchange: HttpExchange): Unit =
    Using(httpExchange) { _ =>
      unsafeHandle(httpExchange)
    }

  private def unsafeHandle(httpExchange: HttpExchange): Unit =
    val uri = httpExchange.getRequestURI
    val method = httpExchange.getRequestMethod
    val path = uri.getPath

    method match
      case "GET" =>
        toValidFile(path) match
          case Some(file) if file.isFile =>
            sendFile(httpExchange, file)
          case Some(file) if file.isDirectory =>
            val htmlPage = makeHtmlPage(file.listFiles(), file)
            reply(httpExchange, 200, htmlPage)
          case _ =>
            reply(httpExchange, 404, "Not found!")
      case "POST" if allowFileUploading =>
        toValidFile(path) match
          case Some(file) if file.isDirectory =>
            FileSaver.receiveFiles(file, httpExchange)
            val htmlPage = makeHtmlPage(file.listFiles(), file)
            reply(httpExchange, 200, htmlPage)
          case _ =>
            reply(httpExchange, 404, "Not found!")
      case _ =>
        reply(httpExchange, 400, "only GET method supported!")

  private def sendFile(httpExchange: HttpExchange, file: File): Unit =
    httpExchange.sendResponseHeaders(200, Files.size(file.toPath))
    Using.Manager { use =>
      val outputStream = use(httpExchange.getResponseBody)
      val inputStream = use(new FileInputStream(file))
      inputStream.transferTo(outputStream)
    }

  def prettySize(file: File): Option[String] =
    if (!file.isFile) return None
    val size: Long = Files.size(file.toPath)
    if (size < 2 * 1024) return Some(s"$size B")
    if (size < 2 * 1024 * 1024) return Some(s"${size / 1024} kB")
    if (size < 2L * 1024 * 1024 * 1024) return Some(s"${size / (1024 * 1024)} MB")
    Some(s"${size / (1024 * 1024 * 1024)} GB")

  def orderFiles(allFiles: Iterable[File]): Iterable[File] =
    allFiles.map(f => (f.isFile, f.getName, f)).toArray.sortBy(triple => (triple._1, triple._2)).map(_._3)

  def makeParentLink(currentDir: File): String =
    toBrowserPath(currentDir) match
      case None => ""
      case Some(browserPath) =>
        val parts = browserPath.split("/")
        val encodedParts = parts.map(encodePathAsLink)
        val links = (2 to parts.size).map { i =>
          val link = encodedParts.take(i).mkString("/")
          val name = parts(i - 1)
          s"""<a href="$link">$name</a>"""
        }.mkString("/")
        f"""<p>$links</p>""".indent(6)

  def makeHtmlPage(files: Iterable[File], currentDir: File): String =
    val links = orderFiles(files).flatMap { file =>
      toBrowserPath(file).map(encodePathAsLink).map { link =>
        f"""<li><a href="$link">${file.getName}</a>${prettySize(file).map(s => s" $s").getOrElse("")}</li>""".indent(8)
      }
    }.mkString("<ul>\n".indent(6), "\n", "\n" + "</ul>".indent(6))

    val parentLink = makeParentLink(currentDir)

    val uploadFile = if (allowFileUploading) {
      """    <form enctype="multipart/form-data" method="post">
        |      <p><input type="file" name="upload" multiple>
        |      <input type="submit" value="Upload"></p>
        |    </form>
        |""".stripMargin
    } else ""

    s"""
       |<!DOCTYPE html>
       |<html>
       |  <head>
       |    <meta charset="utf-8">
       |    <title>${currentDir.getName}</title>
       |  </head>
       |  <body>
       |    <div>
       |$parentLink
       |$links
       |$uploadFile
       |    </div>
       |  </body>
       |</html>
       |""".stripMargin
