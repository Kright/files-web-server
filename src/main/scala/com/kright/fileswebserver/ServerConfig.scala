package com.kright.fileswebserver

import com.electronwill.nightconfig.core.Config
import com.electronwill.nightconfig.core.file.FileConfig

import java.util
import scala.jdk.CollectionConverters.*
import scala.util.Using

case class ServerConfig(port: Int,
                        maxThreadsCount: Int,
                        failIfMappedDirectoryNotExists: Boolean,
                        generateIndexPage: Boolean,
                        mappings: Seq[DirectoryMapping])

object ServerConfig:
  def apply(conf: Config): ServerConfig =
    ServerConfig(
      port = conf.getInt("port"),
      maxThreadsCount = conf.getInt("maxThreadsCount"),
      generateIndexPage = conf.getOrElse[Boolean]("generateIndexPage", false),
      failIfMappedDirectoryNotExists = conf.get[Boolean]("failIfMappedDirectoryNotExists"),
      mappings = conf.get[java.util.ArrayList[Config]]("mapping").asScala.map(DirectoryMapping.apply).toSeq
    )

  def apply(): ServerConfig =
    Using(FileConfig.of("config.toml")) { conf =>
      conf.load()
      ServerConfig(conf)
    }.get


case class DirectoryMapping(fsPath: String,
                            browserPath: String,
                            allowFileUploading: Boolean)

object DirectoryMapping:
  def apply(conf: Config): DirectoryMapping =
    DirectoryMapping(
      fsPath = conf.get[String]("fsPath"),
      browserPath = conf.get[String]("browserPath"),
      allowFileUploading = conf.getOrElse[Boolean]("allowFileUploading", false),
    )
