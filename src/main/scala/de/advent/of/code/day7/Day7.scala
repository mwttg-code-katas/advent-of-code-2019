package de.advent.of.code.day7

import de.advent.of.code.common.FileUtilities

import scala.annotation.tailrec

final case class Input(sourceCode: Vector[Int], phaseSettingSequence: Vector[Int])

object Day7 {

  def main(args: Array[String]): Unit = {
    solvePuzzle1()
  }

  private def solvePuzzle1(): Unit = {
    val sourceCode = readProgram

    val permutations = allPermutationsOf(Vector(0, 1, 2, 3, 4))
    val results = permutations.map(phaseSequence => {
      executeNSerializedIntComputers(sourceCode, phaseSequence)
    })
    val max = results.max
    println(max)
  }

  private def executeNSerializedIntComputers(sourceCode: Vector[Int], phaseSequence: Vector[Int]) = {
    @tailrec
    def helper(currentIteration: Int, accumulator: Int): Int =
      if (currentIteration == phaseSequence.size) {
        accumulator
      } else {
        val phase  = phaseSequence(currentIteration)
        val result = IntComputer.execute(sourceCode, Vector(phase, accumulator))
        helper(currentIteration + 1, result)
      }
    helper(0, 0)
  }

  private def allPermutationsOf(input: Vector[Int]) =
    input.permutations.toVector

  private def readProgram = {
    val lines = FileUtilities.read("day7/input.txt")
    lines.flatMap(line => {
      val numbers = line.split(",").toVector
      numbers.map(_.toInt)
    })
  }

  // Max thruster signal 43210
  private def testInput1 =
    Input(
      Vector(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0),
      Vector(4, 3, 2, 1, 0)
    )

  // Max thruster signal 54321
  private def testInput2 =
    Input(
      Vector(3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0),
      Vector(0, 1, 2, 3, 4)
    )

  // Max thruster signal 65210
  private def testInput3 =
    Input(
      Vector(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4,
        31, 99, 0, 0, 0),
      Vector(1, 0, 4, 3, 2)
    )

  // Max thruster signal 139629729
  private def testInput4 =
    Input(
      Vector(3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5),
      Vector(9, 8, 7, 6, 5)
    )
}
