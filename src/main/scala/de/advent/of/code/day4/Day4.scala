package de.advent.of.code.day4

object Day4 {
  def main(args: Array[String]): Unit = {
    println(solvePuzzle1(265275, 781584))
    println(solvePuzzle2(265275, 781584))
  }

  def solvePuzzle2(start: Int, end: Int): Int = {
    var possibilities = 0
    for (code <- start to end) {
      val digits = code.toString.map(_.asDigit).toVector
      val increasing = validateIncreasingNumbers(digits)
      val adjacenct = validateExactlyTwoAdjacent(digits)
      (increasing, adjacenct) match {
        case (true, true) => possibilities = possibilities + 1
        case (_, _)       => possibilities = possibilities + 0
      }
    }
    possibilities
  }

  private def validateExactlyTwoAdjacent(digits: Vector[Int]) = {
    final case class Accumulator(lastNumber: Int, adj: Map[Int, Int])

    val result = digits.foldLeft(Accumulator(0, Map.empty[Int, Int])) {
      (accumulator, digit) =>
      {
        if (accumulator.lastNumber == digit) {
          if(accumulator.adj.contains(digit)) {
            val amount = accumulator.adj(digit)
            val newAdj = accumulator.adj + (digit -> (amount + 1))
            accumulator.copy(lastNumber = digit, adj = newAdj)
          } else {
            val newAdj = accumulator.adj + (digit -> 2)
            accumulator.copy(lastNumber = digit, adj = newAdj)
          }
        } else {
          accumulator.copy(
            lastNumber = digit
          )
        }
      }
    }
    val r = result.adj.values.filter(x => x == 2)
    r.nonEmpty
  }

  def solvePuzzle1(start: Int, end: Int): Int = {
    var possibilities = 0
    for (code <- start to end) {
      val digits = code.toString.map(_.asDigit).toVector
      val increasing = validateIncreasingNumbers(digits)
      val adjacenct = validateTwoAdjacent(digits)
      (increasing, adjacenct) match {
        case (true, true) => possibilities = possibilities + 1
        case (_, _)       => possibilities = possibilities + 0
      }
    }
    possibilities
  }

  private def validateIncreasingNumbers(digits: Vector[Int]) = {
    final case class Accumulator(lastNumber: Int, isIncreasing: Boolean)

    val result = digits.foldLeft(Accumulator(0, true)) { (accumulator, digit) =>
      {
        if (accumulator.lastNumber <= digit) {
          accumulator.copy(
            lastNumber = digit,
            isIncreasing = accumulator.isIncreasing
          )
        } else {
          accumulator.copy(lastNumber = digit, isIncreasing = false)
        }
      }
    }
    result.isIncreasing
  }

  private def validateTwoAdjacent(digits: Vector[Int]) = {
    final case class Accumulator(lastNumber: Int, isSameNumber: Boolean)

    val result = digits.foldLeft(Accumulator(0, false)) {
      (accumulator, digit) =>
        {
          if (accumulator.lastNumber == digit) {
            accumulator.copy(lastNumber = digit, isSameNumber = true)
          } else {
            accumulator.copy(
              lastNumber = digit,
              isSameNumber = accumulator.isSameNumber
            )
          }
        }
    }
    result.isSameNumber
  }
}
