package aoc2021

object Day02b extends App with FileInput("day02.txt") {

  sealed trait Command
  case class Forward(dist: Int) extends Command
  case class Down(delta: Int) extends Command
  case class Up(delta: Int) extends Command

  case class Position(x: Int, y: Int, aim: Int)

  val commands: Iterator[Command] = inputLines().map { line =>
    line.split(' ') match {
      case Array("forward", dist) => Forward(dist.toInt)
      case Array("down", delta) => Down(delta.toInt)
      case Array("up", delta) => Up(delta.toInt)
      case _ => throw new Exception("invalid instruction")
    }
  }

  val position = commands.foldLeft(Position(0, 0, 0)) { case (p, cmd) =>
    cmd match {
      case Forward(dist) => p.copy(x = p.x + dist, y = p.y + (p.aim * dist))
      case Down(delta) => p.copy(aim = p.aim + delta)
      case Up(delta) => p.copy(aim = p.aim - delta)
    }
  }

  println(position.x * position.y)

}
