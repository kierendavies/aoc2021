package aoc2021

import aoc2021.{FileInput, Input}

import scala.io.StdIn

object Day01b extends App with FileInput("day01.txt") {
  val depths = inputLines().map(_.toInt)
  val smoothedDepths = depths.sliding(3).map(_.sum)
  val increaseCount = smoothedDepths.sliding(2).count { window =>
    window match {
      case Seq(a, b) => a < b
      case _ => false
    }
  }
  println(increaseCount)
}