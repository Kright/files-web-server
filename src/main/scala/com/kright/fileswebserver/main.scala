package com.kright.fileswebserver

import com.sun.net.httpserver.HttpServer

import java.net.InetSocketAddress
import java.nio.file.Path
import java.util.concurrent.{SynchronousQueue, ThreadPoolExecutor, TimeUnit}

@main
def main(): Unit = {
  val config = ServerConfig()
  val server = HttpServer.create(new InetSocketAddress(config.port), 0)

  // the server won't respond if the connection count is bigger than config.maxThreadsCount
  val executor = new ThreadPoolExecutor(0, config.maxThreadsCount, 60L, TimeUnit.SECONDS, new SynchronousQueue())

  val fileHandlers: Seq[FilesHandler] = createFileHandlers(config).collect {
    case Right(filesHandler) => filesHandler
    case Left(errorMsg) => throw IllegalArgumentException(errorMsg)
  }

  fileHandlers.foreach { handler =>
    server.createContext(handler.browserRootPath, handler)
  }

  if (config.generateIndexPage) {
    val page = IndexPage(fileHandlers)
    server.createContext("/", page)
  }

  server.setExecutor(executor)
  server.start()
}

private def createFileHandlers(config: ServerConfig): Seq[Either[String, FilesHandler]] = {
  val providerFactory: String => Either[String, FSPathProvider] =
    if (config.failIfMappedDirectoryNotExists) FSPathProvider.checked
    else path => Right(FSPathProvider.unchecked(path))

  config.mappings.map { mapping =>
    for {
      fsPathProvider <- providerFactory(mapping.fsPath)
      browserPath <- getValidBrowserPathOrError(mapping)
    } yield new FilesHandler(browserPath, fsPathProvider, mapping.allowFileUploading)
  }
}

private def getValidBrowserPathOrError(mapping: DirectoryMapping): Either[String, Path] =
  if (!mapping.browserPath.startsWith("/")) return Left("browserPath should start from '/'")
  val p = Path.of(mapping.browserPath.substring(1))
  if (p.isAbsolute) return Left("invalid browser path")
  Right(p)
