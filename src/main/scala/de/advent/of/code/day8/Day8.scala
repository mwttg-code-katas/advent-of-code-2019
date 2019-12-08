package de.advent.of.code.day8

import de.advent.of.code.common.FileUtilities

object Day8 {
  def main(args: Array[String]): Unit = {
    solvePuzzle1()
    solvePuzzle2()
  }

  def solvePuzzle2(): Unit = {
    val input = readImageFile
    val image = Image.createImage(input, 25, 6)
    image.decodeImage()
  }

  def solvePuzzle1(): Unit = {
    val input      = readImageFile
    val image      = Image.createImage(input, 25, 6)
    val layerIndex = image.layerWithFewestZeros
    val checksum   = image.checksumForLayer(layerIndex)
    println(checksum)
  }

  private def readImageFile = {
    val lines = FileUtilities.read("day8/input.txt")
    lines.flatMap(line => {
      line.map(_.asDigit).toVector
    })
  }
}
