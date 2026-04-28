package com.kright.fileswebserver

case class FileRange(start: Long, endInclusive: Long) {
  val contentLength: Long = endInclusive - start + 1

  def isValid(fileSize: Long): Boolean =
    start >= 0 && endInclusive < fileSize && start <= endInclusive
}


object FileRange:
  def tryParse(rangeSpec: String, fileSize: Long): Option[FileRange] =
    val parts = rangeSpec.split("-")
    if (parts.isEmpty) {
      return None
    }
    val startOption = parts(0).toLongOption
    if (startOption.isEmpty) {
      return None
    }
    val start = startOption.get
    val endInclusive = if (parts.length > 1 && parts(1).nonEmpty) {
      val number = parts(1).toLongOption
      if (number.isEmpty) return None
      number.get
    } else (fileSize - 1)
    Some(FileRange(start, endInclusive))
