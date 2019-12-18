package de.advent.of.code.day18

import de.advent.of.code.day18.Entity.Entity

final case class Cave(map: Vector[Vector[Entity]]) {
  def get(x: Int, y: Int): Entity =
    map(y)(x)
}

object Cave {
  def empty: Cave = Cave(Vector(Vector.empty[Entity]))
}
