package com.kright.fileswebserver

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}
import scala.util.Using
import scala.util.Try
import java.net.{InetSocketAddress, ServerSocket, Socket, URI, URL, URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{Executors, SynchronousQueue, ThreadPoolExecutor, TimeUnit}
import java.nio.file.{Files, Path, Paths}

@main
def main(): Unit = {
  val config = ServerConfig()
  val server = HttpServer.create(new InetSocketAddress(config.port), 0)

  //  val executor = Executors.newCachedThreadPool()
  // server won't respond if connections count is bigger than config.maxThreadsCount
  val executor = new ThreadPoolExecutor(0, config.maxThreadsCount, 60L, TimeUnit.SECONDS, new SynchronousQueue())

  for (mapping <- config.mappings) {
    server.createContext(mapping.browserPath, new FilesHandler(mapping))
  }

  server.setExecutor(executor)
  server.start()
}

private class FilesHandler(mapping: DirectoryMapping) extends HttpHandler :
  private val dirPath: Path = {
    val booksDir = new File(mapping.fsPath)
    require(booksDir.exists(), s"file $booksDir doesn't exist!")
    booksDir.toPath.toAbsolutePath
  }

  private val browserPath: Path = {
    require(mapping.browserPath.startsWith("/"), "browserPath should start from '/'")
    val p = Path.of(mapping.browserPath.substring(1))
    require(!p.isAbsolute, "invalid browser path")
    p
  }

  private def reply(httpExchange: HttpExchange, code: Int, text: String): Unit =
    val response = text.getBytes
    httpExchange.sendResponseHeaders(code, response.length)
    Using(httpExchange.getResponseBody) { outputStream =>
      outputStream.write(response)
    }

  private def toValidFile(pathString: String): Option[File] =
    if (!pathString.startsWith("/")) return None
    val path = Path.of(pathString.substring(1))
    if (path.isAbsolute) return None

    val relativePath = browserPath.relativize(path)

    val absolutePath = dirPath.resolve(relativePath).toAbsolutePath
    if (!absolutePath.startsWith(dirPath)) return None

    val file = absolutePath.toFile
    if (!file.exists()) return None
    Some(file)

  private def toRelativeLink(file: File): Option[String] =
    val absPath = file.toPath.toAbsolutePath
    if (!absPath.startsWith(dirPath)) return None
    val relativePath = dirPath.relativize(absPath)
    val str = s"/${browserPath.resolve(relativePath).toString}"
    // it's better to use UrlEscapers.urlFragmentEscaper().escape(inputString);
    val encoded = str.split("/").map(
      URLEncoder.encode(_, StandardCharsets.UTF_8).replace("+", "%20")
    ).mkString("/")
    Option(encoded)

  override def handle(httpExchange: HttpExchange): Unit =
    Using(httpExchange) { _ =>
      unsafeHandle(httpExchange)
    }

  private def unsafeHandle(httpExchange: HttpExchange): Unit =
    val uri = httpExchange.getRequestURI
    val method = httpExchange.getRequestMethod
    val path = uri.getPath

    if (method != "GET") {
      reply(httpExchange, 400, "only GET method supported!")
      return
    }

    toValidFile(path) match
      case Some(file) if file.isFile => sendFile(httpExchange, file)
      case Some(file) if file.isDirectory =>
        val htmlPage = makeHtmlPage(file.listFiles(), Option(file.getParentFile))
        reply(httpExchange, 200, htmlPage)
      case _ =>
        reply(httpExchange, 404, "Not found!")

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

  def makeHtmlPage(files: Iterable[File], parentDir: Option[File]): String =
    val links = orderFiles(files).flatMap { file =>
      toRelativeLink(file).map { link =>
        f"""<li><a href="$link">${file.getName}</a>${prettySize(file).map(s => s" $s").getOrElse("")}</li>""".indent(8)
      }
    }.mkString("<ul>\n".indent(6), "\n", "\n" + "</ul>".indent(6))

    val parentLink = parentDir.flatMap(toRelativeLink).map { link =>
      f"""<p><a href="$link">..</a></p>""".indent(6)
    }.getOrElse("")

    s"""
       |<!DOCTYPE html>
       |<html>
       |  <head>
       |    <meta charset="utf-8">
       |    <title>This is a title</title>
       |  </head>
       |  <body>
       |    <div>
       |$parentLink
       |$links
       |    </div>
       |  </body>
       |</html>
       |""".stripMargin
