package com.kright.fileswebserver

import com.electronwill.nightconfig.core.Config
import com.electronwill.nightconfig.core.file.FileConfig
import scala.jdk.CollectionConverters._

import java.util
import scala.util.Using

case class ServerConfig(port: Int,
                        maxThreadsCount: Int,
                        mappings: Seq[DirectoryMapping])

case class DirectoryMapping(fsPath: String,
                            browserPath: String)

object ServerConfig:
  def apply(): ServerConfig =
    Using(FileConfig.of("config.toml")) { conf =>
      conf.load()

      ServerConfig(
        port = conf.getInt("port"),
        maxThreadsCount = conf.getInt("maxThreadsCount"),
        mappings = conf.get[java.util.ArrayList[Config]]("mapping").asScala.map { c =>
          DirectoryMapping(
            fsPath = c.get[String]("fsPath"),
            browserPath = c.get[String]("browserPath")
          )
        }.toSeq
      )
    }.get
