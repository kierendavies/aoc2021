package aoc2021

import aoc2021.Input.readInputLines
import cats._
import cats.effect._
import cats.implicits._

object Day03a extends IOApp.Simple:

  def solve(inputLines: List[String]): Int =
    val bitCounts = inputLines.transpose.map(_.groupBy(identity).mapValues(_.size))

    val mostCommonBits = bitCounts.map(_.maxBy(_._2)._1)
    val gamma = Integer.parseInt(mostCommonBits.mkString, 2)

    val leastCommonBits = bitCounts.map(_.minBy(_._2)._1)
    val epsilon = Integer.parseInt(leastCommonBits.mkString, 2)

    gamma * epsilon

  def run =
    for {
      inputLines <- readInputLines("day03.txt")
      solution = solve(inputLines)
      _ <- IO.println(solution)
    } yield ()
