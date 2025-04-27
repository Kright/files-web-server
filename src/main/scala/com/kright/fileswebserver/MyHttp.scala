package com.kright.fileswebserver

import com.sun.net.httpserver.HttpExchange

import scala.util.Using

object MyHttp:
  def reply(httpExchange: HttpExchange, code: Int, text: String): Unit =
    val response = text.getBytes
    httpExchange.sendResponseHeaders(code, response.length)
    Using(httpExchange.getResponseBody) { outputStream =>
      outputStream.write(response)
    }
