package de.advent.of.code.day18

object Day18 {
  def main(args: Array[String]): Unit = {
    solvePuzzle1()
  }

  private def solvePuzzle1(): Unit = {
    val map = Map.from("day18/input.txt")
    map.prettyPrint()
    println(map)
  }

}
