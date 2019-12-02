package de.advent.of.code.common

import scala.io.Source
import scala.util.{Failure, Success, Try}

object FileUtilities {
  def read(filename: String): Vector[String] =
    Try(Source.fromResource(filename).getLines().toVector) match {
      case Success(lines) => lines
      case Failure(exception) =>
        println("Something went wrong. Cause:", exception.printStackTrace())
        Vector.empty
    }
}
