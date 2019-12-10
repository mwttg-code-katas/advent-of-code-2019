package de.advent.of.code.day10

final case class Position(x: Int, y: Int)

final case class AsteroidsMap(width: Int, height: Int, asteroids: Vector[Vector[Boolean]]) {
  def isAsteroid(x: Int, y: Int): Boolean = asteroids(y)(x)

  def allAsteroidsPositions(): Set[Position] = {
    val asteroids = for (y <- 0 until height) yield {
      for (x <- 0 until width) yield {
        if (isAsteroid(x, y)) {
          Some(Position(x, y))
        } else {
          None
        }
      }
    }
    asteroids.flatten.flatten.toSet
  }
}

object AsteroidsMap {

  def isBlockedBy(source: Position, target: Position, allAsteroidPositions: Set[Position]): Set[Position] = {
    val maybeBlocks = allAsteroidPositions.diff(Set(source, target))

    val horizontalBlocks = getHorizontalBlocks(source, target, maybeBlocks)
    val linearFuncBlocks = getLinearFuncBlocks(source, target, maybeBlocks)

    horizontalBlocks.union(linearFuncBlocks)
  }

  private def getLinearFuncBlocks(source: Position, target: Position, maybeBlocks: Set[Position]) = {
    val m = (target.y - source.y) / (target.x - source.x).toFloat
    val n = source.y - (m * source.x)

    maybeBlocks
      .filter(asteroid => asteroid.y == ((m * asteroid.x) + n).toInt) // y = m * x + n
      .flatMap(item => {
        if (isBetweenSourceAndTarget(item, source, target)) {
          Some(item)
        } else {
          None
        }
      })
  }

  private def getHorizontalBlocks(source: Position, target: Position, maybeBlocks: Set[Position]) =
    if (source.x == target.x) {
      maybeBlocks
        .filter(asteroid => asteroid.x == source.x)
        .flatMap(item => {
          if (isBetweenSourceAndTarget(item, source, target)) {
            Some(item)
          } else {
            None
          }
        })
    } else {
      Set.empty[Position]
    }

  private def isBetweenSourceAndTarget(asked: Position, source: Position, target: Position) =
    ((source.x < asked.x && target.x > asked.x) || (source.x > asked.x && target.x < asked.x)) &&
    ((source.y < asked.y && target.y > asked.y) || (source.y > asked.y && target.y < asked.y))

  def createMap(lines: Vector[String]): AsteroidsMap = {
    val map = lines.map(line => {
      line
        .map(char => {
          if (char == '.') {
            false
          } else {
            true
          }
        })
        .toVector
    })
    val height = map.size
    val width  = map(0).size
    AsteroidsMap(width, height, map)
  }
}
