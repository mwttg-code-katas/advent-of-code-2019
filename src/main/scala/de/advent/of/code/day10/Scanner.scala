package de.advent.of.code.day10

object Scanner {

  def scan(asteroidsMap: AsteroidsMap) = {
    val allAsteroids = asteroidsMap.allAsteroidsPositions()

    val whoSeesWhatNOT =
      for (source <- allAsteroids) yield {

        val sourceCanNOTSee =
          for (target <- allAsteroids.diff(Set(source))) yield {

            val blocks = AsteroidsMap.isBlockedBy(source, target, allAsteroids)
            if (blocks.isEmpty) {
              None
            } else {
              Some(target)
            }
          }
        val invisibleForSource = sourceCanNOTSee.flatten
        (source, invisibleForSource)
      }

    val table  = whoSeesWhatNOT.map(item => (item._1, item._2.size)).toMap
    val minBy  = table.map(item => (item._2, item._1))
    val min    = minBy.keys.min
    val key    = minBy.get(min)
    println(table)
    println(s"$key -- $min")
    val blockSize = table.get(key.get).get
    allAsteroids.size - blockSize - 1
  }
}
