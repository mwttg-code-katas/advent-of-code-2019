package de.advent.of.code.day16

object InputDecoder {
  def from(lines: Vector[String]): List[List[Long]] =
    lines.toList.map(line => line.map(_.asDigit.toLong).toList)
}
