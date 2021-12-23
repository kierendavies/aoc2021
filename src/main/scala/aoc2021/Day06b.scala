package aoc2021

import aoc2021.Input.readInputLines
import cats.*
import cats.effect.*
import cats.implicits.*

object Day06b extends IOApp:

  def solve(inputLines: List[String]): Option[BigInt] =
    val iterations = 256

    for {
      line <- inputLines.headOption
      fishTimers <- line.split(',').toList.traverse(_.toIntOption)
      timerGroupsMap = fishTimers.groupMapReduce(identity)(_ => BigInt(1))(_ + _)
      timerGroups = Vector.tabulate(9)(timerGroupsMap.getOrElse(_, BigInt(0)))
      finalTimerGroups = Iterator.iterate(timerGroups) { case groupZero +: remainingGroups =>
        val newGroups = remainingGroups :+ groupZero
        newGroups.updated(6, newGroups(6) + groupZero)
      }.drop(iterations).next
    } yield finalTimerGroups.sum

  def run(args: List[String]): IO[ExitCode] =
    for {
      inputLines <- readInputLines("day06.txt")
      solution = solve(inputLines)
      _ <- solution.fold(IO.unit)(IO.println)
    } yield solution.fold(ExitCode.Error)(_ => ExitCode.Success)
