package com.kright.fileswebserver

object IndexPage:
  def apply(fileHandlers: Seq[FilesHandler]): StaticPage =
    val links = fileHandlers.sortBy(_.browserRootPath).map { handler =>
      f"""<li><a href="${handler.browserRootPath}">${handler.browserPath}</a></li>""".indent(8)
    }.mkString("<ul>\n".indent(6), "\n", "\n" + "</ul>".indent(6))

    StaticPage(
      s"""
         |<!DOCTYPE html>
         |<html>
         |  <head>
         |    <meta charset="utf-8">
         |    <title>index</title>
         |  </head>
         |  <body>
         |    <div>
         |$links
         |    </div>
         |  </body>
         |</html>
         |""".stripMargin
    )
