package de.advent.of.code.day16

import de.advent.of.code.common.FileUtilities

object Day16 {

  def main(args: Array[String]): Unit =
    solvePuzzle1()

  def solvePuzzle1(): Unit = {
    val lines = FileUtilities.read("day16/input.txt")
    // val lines       = Vector("12345678")
    val numberLines = InputDecoder.from(lines)
    val digits     = numberLines.head
    val pattern     = Pattern(List(0, 1, 0, -1))
    val result = FlawedFrequencyTransmission.times(digits, pattern, 100)
    println(result)
  }
}
