package com.github.kmizu.IIdparser

/**
  * Created by kota_mizushima on 2016/09/26.
  */
abstract class IIdParsers {
  val mapping: Array[Array[String]]
  case class Point(x: Int, y: Int)

  def inRange(x: Int, y: Int): Boolean = {
    0 <= x && x < mapping.length &&
    0 <= y && y < mapping.length
  }

  abstract sealed class ParseResult[+T]
  case class ParseSuccess[T](value: T, next: Point) extends ParseResult
  case object ParseFailure extends ParseResult[Nothing]

  abstract class Parser[+T] {
    def apply(input: Point): ParseResult
  }

  def up(content: String): Parser[String] = new Parser[String] {
    def apply(input: Point): ParseResult = {
      val newX = input.x
      val newY = input.y - 1
      if(inRange(newX, newY) && mapping(newX)(newY) == content) {
        ParseSuccess(mapping(newX)(newY), Point(newX, newY))
      }else {
        ParseFailure
      }
    }
  }

  def down(content: String): Parser[String] = new Parser[String] {
    def apply(input: Point): ParseResult = {
      val newX = input.x
      val newY = input.y + 1
      if(inRange(newX, newY) && mapping(newX)(newY) == content) {
        ParseSuccess(mapping(newX)(newY), Point(newX, newY))
      }else {
        ParseFailure
      }
    }
  }

  def right(content: String): Parser[String] = new Parser[String] {
    def apply(input: Point): ParseResult = {
      val newX = input.x + 1
      val newY = input.y
      if(inRange(newX, newY) && mapping(newX)(newY) == content) {
        ParseSuccess(mapping(newX)(newY), Point(newX, newY))
      }else {
        ParseFailure
      }
    }
  }

  def left(content: String): Parser[String] = new Parser[String] {
    def apply(input: Point): ParseResult = {
      val newX = input.x - 1
      val newY = input.y
      if(inRange(newX, newY) && mapping(newX)(newY) == content) {
        ParseSuccess(mapping(newX)(newY), Point(newX, newY))
      }else {
        ParseFailure
      }
    }
  }
}
