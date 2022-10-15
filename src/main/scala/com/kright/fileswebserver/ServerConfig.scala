package com.kright.fileswebserver

import com.electronwill.nightconfig.core.Config
import com.electronwill.nightconfig.core.file.FileConfig
import scala.jdk.CollectionConverters._

import java.util
import scala.util.Using

case class ServerConfig(port: Int,
                        maxThreadsCount: Int,
                        mappings: Seq[DirectoryMapping])

object ServerConfig:
  def apply(conf: Config): ServerConfig =
    ServerConfig(
      port = conf.getInt("port"),
      maxThreadsCount = conf.getInt("maxThreadsCount"),
      mappings = conf.get[java.util.ArrayList[Config]]("mapping").asScala.map(DirectoryMapping.apply).toSeq
    )

  def apply(): ServerConfig =
    FileConfig.builder("conf").build()

    Using(FileConfig.of("config.toml")) { conf =>
      conf.load()
      ServerConfig(conf)
    }.get


case class DirectoryMapping(fsPath: String,
                            browserPath: String)

object DirectoryMapping:
  def apply(conf: Config): DirectoryMapping =
    DirectoryMapping(
      fsPath = conf.get[String]("fsPath"),
      browserPath = conf.get[String]("browserPath")
    )
