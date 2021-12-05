package aoc2021

import aoc2021.Input.readInputLines
import cats._
import cats.effect._
import cats.implicits._

object Day01a extends IOApp.Simple:

  def solve(inputLines: List[String]): Int =
    val depths = inputLines.map(_.toInt)
    val increaseCount = depths.sliding(2).count { window =>
      window match {
        case Seq(a, b) => a < b
        case _         => false
      }
    }
    increaseCount

  def run =
    for {
      inputLines <- readInputLines("day01.txt")
      solution = solve(inputLines)
      _ <- IO.println(solution)
    } yield ()
