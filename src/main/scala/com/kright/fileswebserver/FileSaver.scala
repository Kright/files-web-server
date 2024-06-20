package com.kright.fileswebserver

import com.sun.net.httpserver.HttpExchange

import java.io.{BufferedOutputStream, File, FileOutputStream, InputStream}
import java.nio.charset.StandardCharsets
import scala.language.implicitConversions
import scala.util.chaining.*
import scala.util.{Failure, Success, Try, Using}

private class FileSaver(currentDir: File):
  val buffer = MyRingBuffer(sizePow = 12)

  private val dash: Byte = 0x2D

  private val bytesRN = new Array[Byte](2).tap { arr =>
    arr(0) = 0x0D
    arr(1) = 0x0A
  }

  def process(inputStream: InputStream): Unit = {
    buffer.readMaxData(inputStream)

    val rn = buffer.findRN()
    require(rn != -1)
    require(rn < 60)

    val delimiter = buffer.consumeArray(rn)
    val success = consumeRN()
    require(success)

    while (!(buffer.isEmpty && buffer.isFinished)) {
      processFile(delimiter, inputStream)
    }
  }

  private def processFile(delimiter: Array[Byte], inputStream: InputStream) = {
    val fileName = consumeLineWithRn(256)
      .filter(_.startsWith("Content-Disposition"))
      .flatMap(_.split(" ").find(l => l.startsWith("filename=\"") && l.endsWith("\"")))
      .map(l => l.drop(10).dropRight(1))
      .filter(isValidFileName)

    require(fileName.isDefined)

    val secondLine = consumeLineWithRn(256).filter(_.startsWith("Content-Type"))
    require(secondLine.isDefined)

    val emptyLine = consumeLineWithRn(256).filter(_ == "")
    require(emptyLine.isDefined)

    val newFile = {
      val file = new File(currentDir, fileName.get)
      if (!file.exists()) file else File(currentDir, fileName.get + "_" + System.nanoTime().toString)
    }
    require(!newFile.exists())

    Try {
      writeBytesToFile(newFile, inputStream, delimiter)
    } match
      case Failure(exception) =>
        newFile.delete()
        throw exception
      case Success(_) => ()
  }

  private def writeBytesToFile(file: File, inputStream: InputStream, delimiter: Array[Byte]): Unit = {
    Using.Manager { use =>
      val target = use(new BufferedOutputStream(new FileOutputStream(file)))

      while (true) {
        buffer.readMaxData(inputStream)

        if (buffer.isEmpty && buffer.isFinished) {
          target.flush()
          target.close()
          return
        }

        val rnPos = buffer.findRN()
        rnPos match
          case -1 => {
            require(buffer.validBytes > 1)
            target.write(buffer.consumeArray(buffer.validBytes - 1))
          }
          case 0 => {
            if (buffer.isStartsWith(delimiter, offset = 2)) {
              buffer.consume(2 + delimiter.length)
              if (buffer(0) == 0x2D && buffer(1) == 0x2D && buffer(2) == 0x0D && buffer(3) == 0x0A) {
                buffer.consume(4)
              } else if (buffer(0) == 0x0D && buffer(1) == 0x0A) {
                buffer.consume(2)
              }
              target.flush()
              target.close()
              return
            } else {
              target.write(buffer.consumeArray(2))
            }
          }
          case n => {
            target.write(buffer.consumeArray(rnPos))
          }
      }
    }
  }

  private def isValidFileName(s: String): Boolean = {
    if (s == ".") return false
    if (s == "..") return false
    s.toSet.intersect("/?*|\n\r".toSet).isEmpty
  }

  private def consumeRN(): Boolean =
    val rnPos = buffer.findRN()
    if (rnPos == 0) {
      buffer.consume(2)
      true
    } else {
      false
    }

  private def consumeLineWithRn(maxLineSize: Int): Option[String] =
    val rnPos = buffer.findRN()
    if (rnPos == -1 || rnPos >= maxLineSize) {
      None
    } else {
      val string = new String(buffer.consumeArray(rnPos), StandardCharsets.UTF_8)
      consumeRN()
      Some(string)
    }


object FileSaver:
  def receiveFiles(currentDir: File, httpExchange: HttpExchange): Unit =
    Using.Manager { use =>
      val inputStream = use(httpExchange.getRequestBody)
      val fileSaver = FileSaver(currentDir)

      val result = Try {
        fileSaver.process(inputStream)
      }

      result match
        case Failure(t) => t.printStackTrace()
        case Success(_) =>
    }

