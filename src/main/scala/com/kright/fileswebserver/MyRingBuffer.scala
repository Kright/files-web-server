package com.kright.fileswebserver

import java.io.InputStream

class MyRingBuffer(sizePow: Int):
  val capacity: Int = 1 << sizePow
  val bytes = new Array[Byte](capacity)
  private var startPos: Long = 0
  private var endPos: Long = 0
  private val mask = capacity - 1
  private var _isFinished = false

  def isFinished: Boolean =
    _isFinished

  def validBytes: Int =
    (endPos - startPos).toInt

  def freeBytes: Int =
    capacity - validBytes

  def isEmpty: Boolean =
    endPos == startPos

  def apply(offset: Int): Byte =
    bytes((startPos + offset).toInt & mask)

  def isFull: Boolean =
    validBytes == capacity

  def consume(count: Int): Unit =
    require(count <= validBytes)
    startPos += count

  def consumeArray(count: Int): Array[Byte] =
    require(count <= validBytes)
    require(count >= 0)
    val result = new Array[Byte](count)

    if (count == 0) return result

    val wrappedStart = (startPos & mask).toInt
    val wrappedEnd = ((startPos + count) & mask).toInt

    if (wrappedEnd > wrappedStart) {
      System.arraycopy(bytes, wrappedStart, result, 0, wrappedEnd - wrappedStart)
    } else {
      val endCount = capacity - wrappedStart
      System.arraycopy(bytes, wrappedStart, result, 0, endCount)
      System.arraycopy(bytes, 0, result, endCount, wrappedEnd)
    }

    startPos += count
    result

  def isStartsWith(example: Array[Byte], offset: Int = 0): Boolean =
    require(example.length <= capacity)
    if (validBytes < offset + example.length) return false
    example.zipWithIndex.forall((b, pos) => b == apply(pos + offset))

  def readData(stream: InputStream): Unit =
    val maxRead = freeBytes
    val writeStart = (endPos & mask).toInt

    val newBytesCount = stream.read(bytes, writeStart, Math.min(maxRead, capacity - writeStart))

    if (newBytesCount == -1) {
      _isFinished = true
    } else {
      endPos += newBytesCount
    }

  def readMaxData(stream: InputStream): Unit =
    while (!_isFinished && !isFull) {
      readData(stream)
    }

  def findRN(): Int = {
    var i = 0
    val end = validBytes
    while (i + 2 <= end) {
      if (apply(i + 0) == 0x0D && apply(i + 1) == 0x0A) {
        return i
      }
      i += 1
    }
    -1
  }
