package com.kright.fileswebserver

import java.io.File
import java.nio.file.Path
import scala.util.Try

trait FSPathProvider:
  def getAbsolutePath: Option[Path]


object FSPathProvider:
  def checked(localDir: String): Either[String, FSPathProvider] =
    val file = new File(localDir)
    if (file.exists()) {
      val path = file.toPath.toAbsolutePath
      Right(
        new FSPathProvider:
          override def getAbsolutePath: Option[Path] = Option(path)
      )
    } else Left(s"fs path $localDir doesn't exist")

  def unchecked(localDir: String): FSPathProvider =
    new FSPathProvider:
      override def getAbsolutePath: Option[Path] =
        val file = new File(localDir)
        if (file.exists()) Try {file.toPath.toAbsolutePath}.toOption
        else None
