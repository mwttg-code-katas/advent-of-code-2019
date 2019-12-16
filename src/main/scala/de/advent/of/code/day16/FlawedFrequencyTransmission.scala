package de.advent.of.code.day16

import scala.annotation.tailrec

// well that's slow  ^^
object FlawedFrequencyTransmission {
  def nextPhase(digits: List[Long], pattern: Pattern): List[Long] = {
    @tailrec
    def helper(currentDigits: List[Long], currentPattern: Pattern, accumulator: List[Long]): List[Long] =
      currentDigits match {
        case Nil => accumulator
        case head :: tail => {
          currentPattern.modifiedPattern()
          val number = calculateNumber(true, digits, currentPattern, 0)
          helper(
            tail,
            currentPattern,
            accumulator :+ Math.abs(number % 10)
          )
        }
      }

    @tailrec
    def calculateNumber(first: Boolean, digitsForCalculation: List[Long], nPattern: Pattern, accumulator: Long): Long =
      digitsForCalculation match {
        case Nil => accumulator
        case head :: tail =>
          if (first) {
            nPattern.shiftForFirst()
          }
          val x      = nPattern.getDigit
          val result = head * x
          calculateNumber(false, tail, nPattern, accumulator + result)
      }

    helper(digits, pattern, List.empty[Long])
  }

  def times(digits: List[Long], pattern: Pattern, times: Int): List[Long] = {
    @tailrec
    def helper(currentDigits: List[Long], currentN: Int): List[Long] = {
      println(s"calculating phase $currentN")
      if (currentN == times) {
        currentDigits
      } else {
        val nextPhase = FlawedFrequencyTransmission.nextPhase(currentDigits, pattern)
        pattern.reset()
        helper(nextPhase, currentN + 1)
      }
    }

    helper(digits, 0)
  }
}
