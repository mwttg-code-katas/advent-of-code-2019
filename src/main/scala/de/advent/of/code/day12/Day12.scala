package de.advent.of.code.day12

import scala.annotation.tailrec

object Day12 {
  def main(args: Array[String]): Unit = {
    solvePuzzle1()
    solvePuzzle2()
  }

  private def solvePuzzle2(): Unit = {
    val input = InputReader.from("day12/input.txt")
    // val input = InputReader.from("day12/test-input.txt")
    val result = calculateTicksTillExistingState(input)
    println(result)
  }

  // Well I hope you've got time... take a coffee... or two... or three...  ;)
  // :( java.lang.OutOfMemoryError: GC overhead limit exceeded
  // I really need another solution
  private def calculateTicksTillExistingState(moons: List[Moon]) = {
    @tailrec
    def helper(currentMoons: List[Moon], states: Set[Set[Moon]], accumulator: Long): Long = {
      if (states.contains(currentMoons.toSet)) {
        accumulator
      } else {
        // val newState = states + currentMoons.toSet
        val newMoons = Gravity.oneTick(currentMoons)
        helper(newMoons, states, accumulator + 1)
      }
    }

    val tick1 = calculateNTicks(1, moons)
    helper(tick1, Set(moons.toSet), 1)
  }

  private def solvePuzzle1(): Unit = {
    val input = InputReader.from("day12/input.txt")
    // val input = InputReader.from("day12/test-input.txt")
    val endState = calculateNTicks(1000, input)
    val totalEnergy = Gravity.totalEnergy(endState)
    println(totalEnergy)
  }

  private def calculateNTicks(n: Int, moons: List[Moon]) = {
    @tailrec
    def helper(currentN: Int, currentMoons: List[Moon]): List[Moon] = {
      if (currentN == n) {
        currentMoons
      } else {
        val newMoons = Gravity.oneTick(currentMoons)
        helper(currentN + 1, newMoons)
      }
    }

    helper(0, moons)
  }
}
