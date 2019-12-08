package de.advent.of.code.day8

import scala.annotation.tailrec

final case class Image(width: Int, height: Int, data: Vector[Layer]) {
  def layerWithFewestZeros: Int = {
    val zerosPerLayer = data.map(layer => {
      (layer.howMany(0), layer.layerIndex)
    })
    val minAmountZeros = zerosPerLayer.min

    // layerIndex
    minAmountZeros._2
  }

  def checksumForLayer(layerIndex: Int): Int = {
    val layer = data(layerIndex)
    val ones  = layer.howMany(1)
    val twos  = layer.howMany(2)
    ones * twos
  }

  def decodeImage(): Unit =
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val colors = for (layer <- data.indices) yield {
          data(layer).color(x, y)
        }
        val color = colors.filterNot(item => item == 2)(0)
        print(if (color == 1) 0x2588.toChar else " ")
      }
      println
    }
}

object Image {

  def createImage(input: Vector[Int], width: Int, height: Int): Image = {
    @tailrec
    def helper(in: Vector[Int], layerIndex: Int, accumulator: Image): Image =
      if (in.isEmpty) {
        accumulator
      } else {
        val layerData = in.take(width * height)
        val layer     = Layer.createLayer(layerData, layerIndex, width, height)
        val accImage  = Image(width, height, accumulator.data :+ layer)
        helper(in.drop(width * height), layerIndex + 1, accImage)
      }

    helper(input, 0, Image(width, height, Vector.empty[Layer]))
  }
}
