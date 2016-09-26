package com.github.kmizu.IIdparser

import scala.collection.mutable

/**
  * Created by kota_mizushima on 2016/09/26.
  */
abstract class IIdParsers {
  val mapping: Array[Array[Char]]
  case class Point(x: Int, y: Int)
  case class ~[+A, +B](_1: A, _2: B)

  def inRange(x: Int, y: Int): Boolean = {
    0 <= x && x < mapping.length &&
    0 <= y && y < mapping(0).length
  }

  abstract sealed class ParseResult[+T]
  case class ParseSuccess[T](value: T, next: Point) extends ParseResult[T]
  case object ParseFailure extends ParseResult[Nothing]

  abstract class Parser[+T] {self =>
    def apply(input: Point): ParseResult[T]
    def /[U >: T](other: Parser[U]): Parser[U] = parserOf{input =>
      this(input) match {
        case success@ParseSuccess(_, _) => success
        case ParseFailure => other(input)
      }
    }
    def ~[U](other: Parser[U]): Parser[T ~ U] = parserOf{input =>
      this(input) match {
        case success1@ParseSuccess(_, _) =>
          val next = success1.next
          other(next) match {
            case success2@ParseSuccess(_, _) =>
              ParseSuccess(new ~(success1.value, success2.value), success2.next)
            case ParseFailure => ParseFailure
          }
        case ParseFailure => ParseFailure
      }
    }
    def * : Parser[List[T]] = parserOf{input =>
      def repeat(rest: Point, buffer: mutable.Buffer[T]): ParseResult[List[T]] = {
        this(rest) match {
          case ParseSuccess(value, next) =>
            buffer.append(value)
            repeat(next, buffer)
          case ParseFailure =>
            ParseSuccess(buffer.toList, rest)
        }
      }
      repeat(input, mutable.Buffer[T]())
    }
  }

  def parserOf[T](block: Point => ParseResult[T]): Parser[T] = new Parser[T] {
    def apply(input: Point): ParseResult[T] = block(input)
  }

  def up(content: Char): Parser[Char] = parserOf{input =>
    val newX = input.x
    val newY = input.y - 1
    if(inRange(newX, newY) && mapping(newX)(newY) == content) {
      ParseSuccess(mapping(newX)(newY), Point(newX, newY))
    }else {
      ParseFailure
    }
  }

  def upAny: Parser[Char] = parserOf{input =>
    val newX = input.x
    val newY = input.y - 1
    if(inRange(newX, newY)) {
      ParseSuccess(mapping(newX)(newY), Point(newX, newY))
    }else {
      ParseFailure
    }
  }

  def down(content: Char): Parser[Char] = parserOf{input =>
    val newX = input.x
    val newY = input.y + 1
    if(inRange(newX, newY) && mapping(newX)(newY) == content) {
      ParseSuccess(mapping(newX)(newY), Point(newX, newY))
    }else {
      ParseFailure
    }
  }

  def downAny: Parser[Char] = parserOf{input =>
    val newX = input.x
    val newY = input.y + 1
    if(inRange(newX, newY)) {
      ParseSuccess(mapping(newX)(newY), Point(newX, newY))
    }else {
      ParseFailure
    }
  }

  def right(content: Char): Parser[Char] = parserOf{input =>
    val newX = input.x + 1
    val newY = input.y
    if(inRange(newX, newY) && mapping(newX)(newY) == content) {
      ParseSuccess(mapping(newX)(newY), Point(newX, newY))
    }else {
      ParseFailure
    }
  }

  def rightAny: Parser[Char] = parserOf{input =>
    val newX = input.x + 1
    val newY = input.y
    if(inRange(newX, newY)) {
      ParseSuccess(mapping(newX)(newY), Point(newX, newY))
    }else {
      ParseFailure
    }
  }

  def left(content: Char): Parser[Char] = parserOf{input =>
    val newX = input.x - 1
    val newY = input.y
    if(inRange(newX, newY) && mapping(newX)(newY) == content) {
      ParseSuccess(mapping(newX)(newY), Point(newX, newY))
    }else {
      ParseFailure
    }
  }
  def leftAny: Parser[Char] = parserOf{input =>
    val newX = input.x - 1
    val newY = input.y
    if(inRange(newX, newY)) {
      ParseSuccess(mapping(newX)(newY), Point(newX, newY))
    }else {
      ParseFailure
    }
  }
}
