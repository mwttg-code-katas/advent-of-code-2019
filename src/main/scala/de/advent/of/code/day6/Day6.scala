package de.advent.of.code.day6

import de.advent.of.code.common.FileUtilities

import scala.annotation.tailrec

final case class OrbitNeighbours(object1: String, object2: String) {
  def findBefore(map: Set[OrbitNeighbours]): OrbitNeighbours = {
    map.find(item => item.object2 == object1).get
  }
}

final case class Level(index: Int, levelItems: Set[OrbitNeighbours])

final case class OrbitNeighboursWithLevel(neighbours: OrbitNeighbours,
                                          index: Int) {
  def backtraceToCom(levels: Vector[Level]): Set[OrbitNeighboursWithLevel] = {
    @tailrec
    def helper(
      current: OrbitNeighboursWithLevel,
      accumulator: Set[OrbitNeighboursWithLevel]
    ): Set[OrbitNeighboursWithLevel] = {
      if (current.index == 0) {
        accumulator
      } else {
        val newCurrent = current.backtrace(levels)

        helper(newCurrent, accumulator + newCurrent)
      }
    }

    helper(this, Set.empty[OrbitNeighboursWithLevel])
  }

  def backtrace(levels: Vector[Level]): OrbitNeighboursWithLevel = {
    val backTo = neighbours.object1
    val inSameLevel =
      levels(index).levelItems.find(item => item.object2 == backTo)
    val levelBefore =
      levels(index - 1).levelItems.find(item => item.object2 == backTo)

    (inSameLevel, levelBefore) match {
      case (Some(value), None) => OrbitNeighboursWithLevel(value, index)
      case (None, Some(value)) => OrbitNeighboursWithLevel(value, index - 1)
    }
  }
}

object Day6 {
  def main(args: Array[String]): Unit = {
    val map = readMap
    println(solvePuzzle1(map))
    println(solvePuzzle2(map))
  }

  private def solvePuzzle2(map: Set[OrbitNeighbours]) = {
    val levels = createTreeLevels(map)
    val youStart = getLevelIndexOf("YOU", levels)
    val sanStart = getLevelIndexOf("SAN", levels)

    val youBacktrace = youStart.backtraceToCom(levels)
    val sanBacktrace = sanStart.backtraceToCom(levels)

    val same = sanBacktrace.intersect(youBacktrace)

    val youNew = youBacktrace.diff(same)
    val sanNew = sanBacktrace.diff(same)

    youNew.size + sanNew.size
  }

  private def getLevelIndexOf(objectName: String, levels: Vector[Level]) = {
    levels
      .flatMap(level => {
        val neighbours =
          level.levelItems.find(item => item.object2 == objectName)
        val index = level.index

        neighbours match {
          case Some(value) => Some(OrbitNeighboursWithLevel(value, index))
          case None        => None
        }
      })
      .head
  }

  private def count(levels: Vector[Level]) = {
    val forCounting = levels.reverse
    val y = forCounting.map(i => (i.index + 1) * i.levelItems.size)
    y.sum
  }

  private def solvePuzzle1(map: Set[OrbitNeighbours]) = {
    val levels = createTreeLevels(map)
    count(levels)
  }

  private def createTreeLevels(map: Set[OrbitNeighbours]) = {
    // well :-S this is mutable and not functional :'(
    val root = map.find(item => item.object1 == "COM").get
    var result = Vector(Level(0, Set(root)))
    var stop = false
    var index = 0

    do {
      val level = result(index)
      val newLevel = createLevel(level, map)
      if (newLevel.isEmpty) {
        stop = true
      } else {
        index = index + 1
        result = result :+ Level(index, newLevel)
      }
    } while (!stop)

    result
  }

  private def createLevel(level: Level, map: Set[OrbitNeighbours]) = {
    level.levelItems.flatMap(item => {
      val objName = item.object2
      map.filter(_.object1 == objName)
    })
  }

  private def readMap = {
    val lines = FileUtilities.read("day6/input.txt")
    // val lines = testLines
    // val lines = testLines2

    lines
      .map(line => {
        val parts = line.split(')')
        OrbitNeighbours(parts(0), parts(1))
      })
      .toSet
  }

  private def testLines =
    Vector(
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L"
    )

  private def testLines2 =
    Vector(
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L",
      "K)YOU",
      "I)SAN"
    )
}
