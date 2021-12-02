package aoc2021

import scala.io.StdIn

object Day01a extends App with FileInput("day01.txt") {
  val depths = inputLines().map(_.toInt)
  val increaseCount = depths.sliding(2).count { window =>
    window match {
      case Seq(a, b) => a < b
      case _ => false
    }
  }
  println(increaseCount)
}