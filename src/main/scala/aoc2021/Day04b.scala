package aoc2021

import aoc2021.Input.readInputLines
import cats._
import cats.effect._
import cats.implicits._

object Day04b extends IOApp:

  def boardWins(board: Vector[Vector[Int]], drawSet: Set[Int]): Boolean =
    board.exists(_.forall(drawSet.contains(_))) || board.transpose.exists(
      _.forall(drawSet.contains(_))
    )

  def score(
      board: Vector[Vector[Int]],
      drawSet: Set[Int],
      drawNumber: Int
  ): Int =
    board.flatten.filter(!drawSet.contains(_)).sum * drawNumber

  def solve(inputLines: List[String]): Either[String, Int] =
    val drawOrder = inputLines.head.split(',').map(_.toInt).toList

    val boards = inputLines
      .drop(2)
      .grouped(6)
      .map { lines =>
        lines
          .take(5)
          .map { line =>
            line.trim.split(" +").map(_.toInt).toVector
          }
          .toVector
      }
      .toVector

    val drawSets = LazyList.unfold((drawOrder, Set.empty[Int])) {
      case (drawOrder, set) =>
        if (drawOrder.isEmpty) None
        else
          val newSet = set + drawOrder.head
          Some((newSet, (drawOrder.tail, newSet)))
    }

    val losingBoard = drawSets
      .map { case drawSet =>
        boards.filter(!boardWins(_, drawSet))
      }
      .dropWhile(_.size > 1)
      .headOption
      .flatMap(_.headOption)
      .toRight("No unique losing board")

    losingBoard.flatMap { board =>
      drawOrder.view.zip(drawSets).find { case (_, drawSet) =>
        boardWins(board, drawSet)
      }.map { case (drawNumber, drawSet) =>
        score(board, drawSet, drawNumber)
      }.toRight("Board doesn't win after all numbers drawn")
    }

  def run(args: List[String]): IO[ExitCode] =
    for {
      inputLines <- readInputLines("day04.txt")
      solution = solve(inputLines)
      _ <- IO.println(solution.fold(_.show, _.show))
    } yield solution.fold(_ => ExitCode.Error, _ => ExitCode.Success)
