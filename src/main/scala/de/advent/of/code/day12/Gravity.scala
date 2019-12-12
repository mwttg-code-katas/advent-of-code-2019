package de.advent.of.code.day12

import scala.annotation.tailrec

final case class Position(x: Int, y: Int, z: Int)

final case class Velocity(vx: Int, vy: Int, vz: Int)

final case class Moon(position: Position, velocity: Velocity)

object Gravity {
  def totalEnergy(moons: List[Moon]): Int =
    moons
      .map(moon => {
        val kin = Math.abs(moon.position.x) + Math.abs(moon.position.y) + Math.abs(moon.position.z)
        val pot = Math.abs(moon.velocity.vx) + Math.abs(moon.velocity.vy) + Math.abs(moon.velocity.vz)
        kin * pot
      })
      .sum

  def oneTick(moons: List[Moon]): List[Moon] = {
    val x = calculateGravity(moons)
    executeVelocity(x)
  }

  private def executeVelocity(moons: List[Moon]) =
    moons.map(moon => {
      Moon(
        position = Position(
          x = moon.position.x + moon.velocity.vx,
          y = moon.position.y + moon.velocity.vy,
          z = moon.position.z + moon.velocity.vz,
        ),
        velocity = moon.velocity
      )
    })

  private def calculateGravity(moons: List[Moon]) = {
    @tailrec
    def helper(toDoMoons: List[Moon], allMoons: Set[Moon], accumulator: List[Moon]): List[Moon] =
      toDoMoons match {
        case Nil => accumulator
        case currentMoon :: rest =>
          val moonsToCheckWith = allMoons.diff(Set(currentMoon))

          val newCurrentMoon = moonsToCheckWith.foldLeft(currentMoon) { (otherMoon, accumulator) =>
            {
              calculate(otherMoon, accumulator)
            }
          }
          val newAccumulator = accumulator :+ newCurrentMoon
          helper(rest, allMoons, newAccumulator)
      }

    helper(moons, moons.toSet, List.empty[Moon])
  }

  private def calculate(currentMoon: Moon, otherMoon: Moon) =
    Moon(
      position = currentMoon.position,
      velocity = Velocity(
        vx = currentMoon.velocity.vx + velocity(currentMoon.position.x, otherMoon.position.x),
        vy = currentMoon.velocity.vy + velocity(currentMoon.position.y, otherMoon.position.y),
        vz = currentMoon.velocity.vz + velocity(currentMoon.position.z, otherMoon.position.z)
      )
    )

  private def velocity(number1: Int, number2: Int) =
    if (number1 > number2) { -1 } else if (number1 < number2) { 1 } else { 0 }

}
