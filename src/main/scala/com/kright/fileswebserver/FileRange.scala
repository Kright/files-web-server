package com.kright.fileswebserver

case class FileRange(start: Long, endInclusive: Long) {
  val contentLength: Long = endInclusive - start + 1

  def isValid(fileSize: Long): Boolean =
    start >= 0 && endInclusive < fileSize && start <= endInclusive
}


object FileRange:
  def tryParse(rangeSpec: String, fileSize: Long): Option[FileRange] =
    require(fileSize > 0)

    if (rangeSpec.contains(',')) return None

    val parts = rangeSpec.split("-", -1)
    if (parts.size != 2) {
      return None
    }

    val startStr = parts(0)
    val endStr = parts(1)

    if (startStr.isEmpty) {
      if (endStr.isEmpty) {
        None
      } else {
        // -500 means last 500 bytes of file
        endStr.toLongOption
          .filter(_ != 0L)
          .map(last => FileRange(math.max(fileSize - last, 0L), fileSize - 1))
      }
    } else {
      if (endStr.isEmpty) {
        // 500- means from byte 500 to file end
        startStr.toLongOption.map(FileRange(_, fileSize - 1))
      } else {
        // 100-199 means bytes from 100 to 199 inclusive
        for (start <- startStr.toLongOption;
             end <- endStr.toLongOption) yield FileRange(start, end)
      }
    }
