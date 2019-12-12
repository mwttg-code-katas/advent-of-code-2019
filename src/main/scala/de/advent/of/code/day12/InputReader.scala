package de.advent.of.code.day12

import de.advent.of.code.common.FileUtilities

object InputReader {
  private val RegEx = """-?\d+""".r

  def from(filename: String): List[Moon] = {
    val lines = FileUtilities.read(filename)
    lines.map(line => {
      val numbers = RegEx.findAllIn(line).toList

      val position = Position(
        x = numbers(0).toInt,
        y = numbers(1).toInt,
        z = numbers(2).toInt
      )

      val velocity = Velocity(0, 0, 0)

      Moon(position, velocity)
    }).toList
  }
}
