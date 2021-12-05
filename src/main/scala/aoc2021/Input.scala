package aoc2021

import scala.io.Source
import cats.effect.{IO, Resource}

object Input:
  def readInputLines(resourceFileName: String): IO[List[String]] =
    Resource.fromAutoCloseable(IO(Source.fromResource(resourceFileName))).use { inputFile =>
      IO(inputFile.getLines.toList)
    }
