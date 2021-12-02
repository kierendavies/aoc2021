package aoc2021

object Day02a extends App with FileInput("day02.txt") {

  sealed trait Command
  case class Forward(dist: Int) extends Command
  case class Down(dist: Int) extends Command
  case class Up(dist: Int) extends Command

  case class Position(x: Int, y: Int)

  val commands: Iterator[Command] = inputLines().map { line =>
    line.split(' ') match {
      case Array("forward", dist) => Forward(dist.toInt)
      case Array("down", dist) => Down(dist.toInt)
      case Array("up", dist) => Up(dist.toInt)
      case _ => throw new Exception("invalid instruction")
    }
  }

  val position = commands.foldLeft(Position(0, 0)) { case (p, cmd) =>
    cmd match {
      case Forward(dist) => p.copy(x = p.x + dist)
      case Down(dist) => p.copy(y = p.y + dist)
      case Up(dist) => p.copy(y = p.y - dist)
    }
  }

  println(position.x * position.y)

}
