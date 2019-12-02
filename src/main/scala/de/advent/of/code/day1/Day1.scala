package de.advent.of.code.day1

import de.advent.of.code.common.FileUtilities

import scala.annotation.tailrec

object Day1 {
  def main(args: Array[String]): Unit = {
    println(solvePuzzle1)
    println(solvePuzzle2)
  }

  private def solvePuzzle1: Int = {
    val lines = FileUtilities.read("day1/input.txt")
    lines.map(line => {
      val number = line.toInt
      (number / 3) - 2
    }).sum
  }

  private def solvePuzzle2 = {
    val lines = FileUtilities.read("day1/input.txt")
    lines.map(line => {
      val number = line.toInt
      val mass = (number / 3) - 2
      val fuel = fuelForFuel(mass)
      mass + fuel
    }).sum
  }

  private def fuelForFuel(mass: Int) = {
    @tailrec
    def helper(currentMass: Int, accumulator: Int): Int = {
      if ((currentMass / 3) - 2 <= 0) {
        accumulator
      } else {
        val fuel = (currentMass / 3) - 2
        helper(fuel, accumulator + fuel)
      }
    }
    helper(mass, 0)
  }
}
