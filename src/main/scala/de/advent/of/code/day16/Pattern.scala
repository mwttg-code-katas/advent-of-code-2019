package de.advent.of.code.day16

class Pattern(val digits: List[Int]) {
  private var index          = 0
  private var run            = 0
  var currentPattern = digits

  def shiftForFirst(): Unit = {
    incIndex()
    correctIndex()
  }

  def modifiedPattern(): Pattern = {
    run            = run + 1
    index          = 0
    currentPattern = digits.flatMap(digit => List.fill(run)(digit))

    this
  }

  def getDigit: Int = {
    val result = currentPattern(index)
    incIndex()
    correctIndex()

    result
  }

  def reset(): Unit = {
    index          = 0
    run            = 0
    currentPattern = digits
  }

  private def correctIndex(): Unit =
    if (index == currentPattern.size) index = 0

  private def incIndex(): Unit = index = index + 1
}

object Pattern {
  def apply(digits: List[Int]): Pattern = new Pattern(digits)
}
