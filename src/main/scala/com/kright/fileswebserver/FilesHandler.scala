package com.kright.fileswebserver

import com.kright.fileswebserver.MyHttp.reply
import com.sun.net.httpserver.{HttpExchange, HttpHandler}

import java.io.File
import java.net.URLEncoder
import java.nio.channels.{Channels, FileChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.{Try, Using}

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
      case "GET" | "HEAD" =>
        toValidFile(path) match
          case Some(file) if file.isFile =>
            sendFile(httpExchange, file.toPath, method == "HEAD")
          case Some(file) if file.isDirectory =>
            val htmlPage = makeHtmlPage(file.listFiles(), file)
            if (method == "HEAD") {
              val response = htmlPage.getBytes(StandardCharsets.UTF_8)
              httpExchange.getResponseHeaders.set("Content-Type", "text/html; charset=utf-8")
              httpExchange.sendResponseHeaders(200, response.length)
              httpExchange.getResponseBody.close()
            } else {
              reply(httpExchange, 200, htmlPage)
            }
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

  private def sendFile(httpExchange: HttpExchange, file: Path, isHead: Boolean): Unit =
    val fileSize = Files.size(file)
    val responseHeaders = httpExchange.getResponseHeaders
    responseHeaders.add("Accept-Ranges", "bytes")

    Try {
      Files.probeContentType(file)
    }.foreach { contentType =>
      if (contentType != null) {
        responseHeaders.set("Content-Type", contentType)
      }
    }

    val rangeHeader = Option(httpExchange.getRequestHeaders.getFirst("Range"))

    rangeHeader match
      case Some(range) if range.toLowerCase.startsWith("bytes=") =>
        sendRange(httpExchange, file, fileSize, range.substring(6).trim, isHead)
      case _ =>
        sendWholeFile(httpExchange, file, fileSize, isHead)

  private def sendWholeFile(httpExchange: HttpExchange, file: Path, fileSize: Long, isHead: Boolean): Unit =
    httpExchange.sendResponseHeaders(200, fileSize)
    sendFileBytes(file, httpExchange, 0, fileSize, isHead)

  private def sendRange(httpExchange: HttpExchange, file: Path, fileSize: Long, rangeSpec: String, isHead: Boolean): Unit =
    FileRange.tryParse(rangeSpec, fileSize) match {
      case Some(range) if range.isValid(fileSize) => {
        httpExchange.getResponseHeaders.add("Content-Range", s"bytes ${range.start}-${range.endInclusive}/$fileSize")
        httpExchange.sendResponseHeaders(206, range.contentLength)
        sendFileBytes(file, httpExchange, range.start, range.contentLength, isHead)
      }
      case _ => {
        httpExchange.getResponseHeaders.add("Content-Range", s"bytes */$fileSize")
        reply(httpExchange, 416, "Range Not Satisfiable")
      }
    }

  private def sendFileBytes(file: Path, httpExchange: HttpExchange, start: Long, contentLength: Long, isHead: Boolean): Unit =
    if (isHead) {
      httpExchange.getResponseBody.close()
      return
    }
    Using.Manager { use =>
      val outputChannel = use(Channels.newChannel(use(httpExchange.getResponseBody)))
      val fileChannel = use(FileChannel.open(file, StandardOpenOption.READ))

      var position = start
      val targetEnd = start + contentLength
      while (position < targetEnd) {
        position += fileChannel.transferTo(position, targetEnd - position, outputChannel)
      }
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
