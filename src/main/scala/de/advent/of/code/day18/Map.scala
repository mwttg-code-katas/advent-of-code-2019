package de.advent.of.code.day18

import de.advent.of.code.common.FileUtilities
import de.advent.of.code.day18.Entity._

final case class Map(player: Player, cave: Cave, keys: Set[Key], doors: Set[Door]) {
  def prettyPrint(): Unit =
    cave.map.foreach(row => {
      row.foreach(column => {
        if (column == Wall) {
          print(s"${0x2588.toChar}")
        } else {
          print(" ")
        }
      })
      println()
    })
}

object Map {
  import Entity._

  def empty: Map =
    Map(
      player = Player(
        position = Position(x = 0, y = 0),
        backpack = Set.empty[Key]
      ),
      cave  = Cave.empty,
      keys  = Set.empty[Key],
      doors = Set.empty[Door]
    )

  def from(filename: String): Map = {
    val lines = FileUtilities.read(filename)
    create(lines)
  }

  private def create(lines: Vector[String]) =
    lines.foldLeft(Map.empty) { (rowAccumulator, line) =>
      {
        val chars = line.toList
        val rowResult = chars.foldLeft(
          rowAccumulator.copy(
            cave  = Cave.empty,
            keys  = Set.empty[Key],
            doors = Set.empty[Door]
          )
        ) { (columnAccumulator, char) =>
          {
            char match {
              case '#' =>
                val newCurrentMap = createWall(columnAccumulator)
                columnAccumulator.copy(cave = Cave(newCurrentMap))

              case '.' =>
                val newCurrentMap = createOpenPassage(columnAccumulator)
                columnAccumulator.copy(cave = Cave(newCurrentMap))

              case '@' =>
                val player: Player = createPlayer(rowAccumulator, columnAccumulator)
                val newCurrentMap  = createOpenPassage(columnAccumulator)
                columnAccumulator.copy(
                  player = player,
                  cave   = Cave(newCurrentMap)
                )

              case key if key.isLetter && key.isLower =>
                val newCurrentKeys = createKey(rowAccumulator, columnAccumulator, key)
                val newCurrentMap  = createOpenPassage(columnAccumulator)
                columnAccumulator.copy(
                  keys = newCurrentKeys,
                  cave = Cave(newCurrentMap)
                )

              case door if door.isLetter && door.isUpper =>
                val newCurrentDoors = createDoor(rowAccumulator, columnAccumulator, door)
                val newCurrentMap   = createOpenPassage(columnAccumulator)
                columnAccumulator.copy(
                  doors = newCurrentDoors,
                  cave  = Cave(newCurrentMap)
                )
            }
          }
        }

        rowAccumulator.copy(
          player = rowResult.player,
          cave   = Cave(rowAccumulator.cave.map :+ rowResult.cave.map.reverse.head),
          keys   = rowAccumulator.keys.union(rowResult.keys),
          doors  = rowAccumulator.doors.union(rowResult.doors)
        )
      }
    }

  private def createPlayer(rowAccumulator: Map, columnAccumulator: Map) = {
    val y      = rowAccumulator.cave.map.size - 1
    val x      = columnAccumulator.cave.map.reverse.head.size
    val player = Player(Position(x, y), Set.empty[Key])
    player
  }

  private def createKey(rowAccumulator: Map, columnAccumulator: Map, key: Char) = {
    val y              = rowAccumulator.cave.map.size - 1
    val x              = columnAccumulator.cave.map.reverse.head.size
    val currentKeys    = columnAccumulator.keys
    val newCurrentKeys = currentKeys + Key(key, Position(x, y))
    newCurrentKeys
  }

  private def createDoor(rowAccumulator: Map, columnAccumulator: Map, door: Char) = {
    val y               = rowAccumulator.cave.map.size - 1
    val x               = columnAccumulator.cave.map.reverse.head.size
    val currentDoors    = columnAccumulator.doors
    val newCurrentDoors = currentDoors + Door(door, Position(x, y))
    newCurrentDoors
  }

  private def createOpenPassage(columnAccumulator: Map) =
    createMapEntity(columnAccumulator, Space)

  private def createWall(columnAccumulator: Map) =
    createMapEntity(columnAccumulator, Wall)

  private def createMapEntity(columnAccumulator: Map, entity: Entity) = {
    val currentRow    = columnAccumulator.cave.map.reverse.head
    val newCurrentRow = currentRow :+ entity
    val currentMap    = columnAccumulator.cave.map
    val newCurrentMap = currentMap.reverse.tail.reverse :+ newCurrentRow
    newCurrentMap
  }
}
