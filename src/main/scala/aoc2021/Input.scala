package aoc2021

import scala.io.{Source, StdIn}

trait Input {
  def inputLines(): LazyList[String] = LazyList.continually(Option(StdIn.readLine())).takeWhile(_.isDefined).flatten
}

trait FileInput(file: String) {
  def inputLines(): Iterator[String] = Source.fromResource(file).getLines()
}
