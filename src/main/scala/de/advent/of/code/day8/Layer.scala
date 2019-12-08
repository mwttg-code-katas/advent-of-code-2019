package de.advent.of.code.day8

import scala.annotation.tailrec

final case class Layer(layerIndex: Int, data: Vector[Vector[Int]]) {
  def howMany(number: Int): Int = {
    val amountOfNumbersPerRow = data.map(row => row.groupBy(identity).view.mapValues(_.size))
    val numberPerRow          = amountOfNumbersPerRow.flatMap(item => item.get(number))
    numberPerRow.sum
  }

  def color(x: Int, y: Int): Int =
    data(y)(x)
}

object Layer {

  def createLayer(input: Vector[Int], index: Int, width: Int, height: Int): Layer = {
    @tailrec
    def helper(in: Vector[Int], accumulator: Layer): Layer =
      if (in.isEmpty) {
        accumulator
      } else {
        val row   = in.take(width)
        val data  = accumulator.data :+ row
        val layer = Layer(index, data)
        helper(in.drop(width), layer)
      }

    helper(input, Layer(0, Vector.empty[Vector[Int]]))
  }
}
