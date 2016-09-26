package com.github.kmizu.IIdparser

object Main extends IIdParsers {
  override val mapping =
    """|aaaa
       |bbbb
       |cccc
       |dddd
       |eeee""".stripMargin.lines.toArray.map(_.toCharArray)
  def main(args: Array[String]): Unit = {
    val parser = (rightAny / downAny).*
    println(mapping(4)(1))
    println(parser(Point(0, 0)))
  }
}
