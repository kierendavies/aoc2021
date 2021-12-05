package aoc2021

import aoc2021.Input.readInputLines
import cats._
import cats.effect._
import cats.implicits._

object Day02a extends IOApp.Simple:

  enum Command:
    case Forward(dist: Int)
    case Down(dist: Int)
    case Up(dist: Int)

  case class Position(x: Int, y: Int)

  def solve(inputLines: List[String]): Int =
    val commands = inputLines.map {
      case s"forward $dist" => Command.Forward(dist.toInt)
      case s"down $dist"    => Command.Down(dist.toInt)
      case s"up $dist"      => Command.Up(dist.toInt)
    }

    val finalPosition = commands.foldLeft(Position(0, 0)) { case (p, cmd) =>
      cmd match {
        case Command.Forward(dist) => p.copy(x = p.x + dist)
        case Command.Down(dist)    => p.copy(y = p.y + dist)
        case Command.Up(dist)      => p.copy(y = p.y - dist)
      }
    }

    finalPosition.x * finalPosition.y

  def run =
    for {
      inputLines <- readInputLines("day02.txt")
      solution = solve(inputLines)
      _ <- IO.println(solution)
    } yield ()
