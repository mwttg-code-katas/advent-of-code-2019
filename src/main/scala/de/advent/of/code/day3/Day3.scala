package de.advent.of.code.day3

import de.advent.of.code.common.FileUtilities

object Day3 {
  def main(args: Array[String]): Unit = {
    solvePuzzle1()
    solvePuzzle2()
  }

  final case class Point(x: Int, y: Int)

  final case class Accumulator(currentPoint: Point, allPoints: Vector[Point])

  private val down: Point => Point = currentPoint =>
    Point(currentPoint.x, currentPoint.y - 1)
  private val up: Point => Point = currentPoint =>
    Point(currentPoint.x, currentPoint.y + 1)
  private val right: Point => Point = currentPoint =>
    Point(currentPoint.x + 1, currentPoint.y)
  private val left: Point => Point = currentPoint =>
    Point(currentPoint.x - 1, currentPoint.y)

  final case class Distance(wire1: Int, wire2: Int) {
    def sum: Int = wire1 + wire2
  }

  def solvePuzzle2(): Unit = {
     val lines = FileUtilities.read("day3/input.txt")

    val wire1 = createWireCoordinates(lines(0))
    val wire2 = createWireCoordinates(lines(1))
    val intersections = wire1.allPoints.toSet.intersect(wire2.allPoints.toSet)

    val distances = intersections.map(intersection => {
      val intersectionIndexWire1 = wire1.allPoints.indexOf(intersection)
      val intersectionIndexWire2 = wire2.allPoints.indexOf(intersection)

      val subWire1 = wire1.allPoints.take(intersectionIndexWire1 + 1)
      val subWire2 = wire2.allPoints.take(intersectionIndexWire2 + 1)
      val distanceWire1 = distance(subWire1)
      val distanceWire2 = distance(subWire2)
      Distance(distanceWire1.distance + 1, distanceWire2.distance + 1).sum  // +1 because distance fold misses first point ;)
    })

    println(s"Fewest combined steps = ${distances.min}")
  }

  final case class DistanceWithPoint(distance: Int, lastPoint: Point)

  private def distance(points: Vector[Point]) = {
    points.foldLeft(DistanceWithPoint(0, points(0))) { (accumulator, point) =>
      {
        val xlength = Math.abs(accumulator.lastPoint.x - point.x)
        val ylength = Math.abs(accumulator.lastPoint.y - point.y)
        val newDistance = xlength + ylength
        accumulator.copy(
          distance = accumulator.distance + newDistance,
          lastPoint = point
        )
      }
    }
  }

  def solvePuzzle1(): Unit = {
    val lines = FileUtilities.read("day3/input.txt")
    val wire1 = createWireCoordinates(lines(0))
    val wire2 = createWireCoordinates(lines(1))
    val intersections = wire1.allPoints.toSet.intersect(wire2.allPoints.toSet)
    val min = intersections.minBy(point => point.x.abs + point.y.abs)
    println(
      s"Minimum: x = ${min.x} y={min.y} --> distance= ${min.x.abs + min.y.abs}"
    )
  }

  private def createWireCoordinates(line: String) = {
    val instructions = line.split(",").toVector
    instructions.foldLeft(Accumulator(Point(0, 0), Vector.empty[Point])) {
      (accumulator, instruction) =>
        {
          (instruction.take(1), instruction.drop(1).toInt) match {
            case ("R", units) =>
              createAccumulator(accumulator, units, right)
            case ("L", units) =>
              createAccumulator(accumulator, units, left)
            case ("U", units) =>
              createAccumulator(accumulator, units, up)
            case ("D", units) =>
              createAccumulator(accumulator, units, down)
          }
        }
    }
  }

  private def createAccumulator(accumulator: Accumulator,
                                units: Int,
                                func: Point => Point) = {
    val x = createPoints(accumulator.currentPoint, units, func)
    accumulator.copy(
      currentPoint = x.currentPoint,
      allPoints = accumulator.allPoints.union(x.allPoints)
    )
  }

  private def createPoints(point: Point, units: Int, func: Point => Point) = {
    var currentPoint = point
    var points = Vector.empty[Point]
    (0 until units)
      .foreach(_ => {
        currentPoint = func(currentPoint)
        points = points :+ currentPoint
      })
    Accumulator(currentPoint, points)
  }
}
