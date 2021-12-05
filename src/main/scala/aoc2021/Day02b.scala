package aoc2021

import aoc2021.Input.readInputLines
import cats._
import cats.effect._
import cats.implicits._

object Day02b extends IOApp.Simple:

  enum Command:
    case Forward(dist: Int)
    case Down(delta: Int)
    case Up(delta: Int)

  case class Position(x: Int, y: Int, aim: Int)

  def solve(inputLines: List[String]): Int =
    val commands = inputLines.map {
      case s"forward $dist" => Command.Forward(dist.toInt)
      case s"down $delta"   => Command.Down(delta.toInt)
      case s"up $delta"     => Command.Up(delta.toInt)
    }

    val finalPosition = commands.foldLeft(Position(0, 0, 0)) { case (p, cmd) =>
      cmd match {
        case Command.Forward(dist) => p.copy(x = p.x + dist, y = p.y + (p.aim * dist))
        case Command.Down(delta)   => p.copy(aim = p.aim + delta)
        case Command.Up(delta)     => p.copy(aim = p.aim - delta)
      }
    }

    finalPosition.x * finalPosition.y

  def run =
    for {
      inputLines <- readInputLines("day02.txt")
      solution = solve(inputLines)
      _ <- IO.println(solution)
    } yield ()
