package aoc2021

import aoc2021.Input.readInputLines
import cats._
import cats.effect._
import cats.implicits._

object Day05a extends IOApp:

  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point):
    def isAxial: Boolean = start.x == end.x || start.y == end.y

    def enclosedPoints: Seq[Point] =
      def range(start: Int, end: Int): Range =
        if (start <= end) Range.inclusive(start, end) else Range.inclusive(start, end, -1)

      for {
        x <- range(start.x, end.x)
        y <- range(start.y, end.y)
      } yield Point(x, y)

  object Line:
    def read(s: String): Option[Line] = s match {
      case s"$x1Str,$y1Str -> $x2Str,$y2Str" =>
        for {
          x1 <- x1Str.toIntOption
          y1 <- y1Str.toIntOption
          x2 <- x2Str.toIntOption
          y2 <- y2Str.toIntOption
        } yield Line(Point(x1, y1), Point(x2, y2))
      case _ => None
    }


  def solve(inputLines: List[String]): Option[Int] =
    for {
      lines <- inputLines.traverse(Line.read)
      axialLines = lines.filter(_.isAxial)
      pointCounts = axialLines.map { line =>
        line.enclosedPoints.map(_ -> 1).toMap
      }.combineAll
      overlapCount = pointCounts.filter(_._2 >= 2).size
    } yield overlapCount

  def run(args: List[String]): IO[ExitCode] =
    for {
      inputLines <- readInputLines("day05.txt")
      solution = solve(inputLines)
      _ <- solution.fold(IO.unit)(IO.println)
    } yield solution.fold(ExitCode.Error)(_ => ExitCode.Success)
