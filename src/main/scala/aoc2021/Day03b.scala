package aoc2021

import aoc2021.Input.readInputLines
import cats._
import cats.effect._
import cats.implicits._

object Day03b extends IOApp.Simple:

  def filterAtBit(values: List[String], bitIndex: Int)(selectBit: Map[Char, Int] => Char): List[String] =
    val bitCounts = values.map(_(bitIndex)).groupBy(identity).mapValues(_.size).toMap
    val selectedBit = selectBit(bitCounts)
    values.filter(_(bitIndex) == selectedBit)
  
  def findRating(values: List[String])(selectBit: Map[Char, Int] => Char): String =
    LazyList.iterate((values, 0)) { case (values, bitIndex) =>
      (filterAtBit(values, bitIndex)(selectBit), bitIndex + 1)
    }.map(_._1).dropWhile(_.size > 1).head.head

  def solve(inputLines: List[String]): Int =
    val o2Bits = findRating(inputLines)(_.toList.map(_.swap).max._2)
    val o2 = Integer.parseInt(o2Bits, 2)

    val co2Bits = findRating(inputLines)(_.toList.map(_.swap).min._2)
    val co2 = Integer.parseInt(co2Bits, 2)

    o2 * co2

  def run =
    for {
      inputLines <- readInputLines("day03.txt")
      solution = solve(inputLines)
      _ <- IO.println(solution)
    } yield ()
