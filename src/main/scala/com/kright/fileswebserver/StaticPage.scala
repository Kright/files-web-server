package com.kright.fileswebserver

import com.sun.net.httpserver.{HttpExchange, HttpHandler}

import scala.util.Using

class StaticPage(val content: String) extends HttpHandler:
  override def handle(httpExchange: HttpExchange): Unit =
    Using(httpExchange) { _ =>
      val uri = httpExchange.getRequestURI
      val method = httpExchange.getRequestMethod
      val path = uri.getPath

      method match
        case "GET" =>
          MyHttp.reply(httpExchange, 200, content)
        case _ =>
          MyHttp.reply(httpExchange, 400, "only GET method supported!")
    }