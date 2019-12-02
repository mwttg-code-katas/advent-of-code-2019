package de.advent.of.code.day2

import de.advent.of.code.common.FileUtilities

object Day2 {
  def main(args: Array[String]): Unit = {
    println(solvePuzzle1(12, 2))
    println(solvePuzzle2())
  }

  private val add: (Int, Int) => Int = (x, y) => x + y
  private val mul: (Int, Int) => Int = (x, y) => x * y

  private def solvePuzzle1(noun: Int, verb: Int) = {
    var sourceCode = readInput

    sourceCode = sourceCode.updated(1, noun)
    sourceCode = sourceCode.updated(2, verb)

    // unfortunately not functional style
    var stop = false
    var index = 0
    do {
      val op = sourceCode(index)

      if (op == 1) {
        sourceCode = executeOp(add, sourceCode, index)
        index = index + 4
      } else if (op == 2) {
        sourceCode = executeOp(mul, sourceCode, index)
        index = index + 4
      } else if (op == 99) {
        stop = true
      } else {
        println("error!")
      }
    } while (!stop)

    sourceCode(0)
  }

  private def executeOp(func: (Int, Int) => Int,
                        sourceCode: Vector[Int],
                        index: Int) = {
    val pos1 = sourceCode(index + 1)
    val pos2 = sourceCode(index + 2)
    val result = func(sourceCode(pos1), sourceCode(pos2))
    val resultPos = sourceCode(index + 3)
    sourceCode.updated(resultPos, result)
  }

  private def readInput = {
    val lines = FileUtilities.read("day2/input.txt")
    lines.flatMap(line => {
      val numbers = line.split(",").toVector
      numbers.map(_.toInt)
    })
  }

  private def solvePuzzle2(): String = {
    var resultMessage = "No result found"
    for (noun <- 0 to 99) {
      for (verb <- 0 to 99) {
        val result = solvePuzzle1(noun, verb)
        if (result == 19690720) {
          resultMessage = s"noun = $noun - verb = $verb"
        }
      }
    }
    resultMessage
  }
}
