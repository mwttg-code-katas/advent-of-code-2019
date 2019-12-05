package de.advent.of.code.day5

import de.advent.of.code.common.FileUtilities

object Day5 {
  final case class Instruction(opCode: Int,
                               isParam1InPositionMode: Boolean,
                               isParam2InPositionMode: Boolean,
                               isParam3InPositionMode: Boolean,
                               nextInstructionPointer: Int)

  def main(args: Array[String]): Unit = {
    execute
  }

  // nice code has an other appearance :/
  // looks like the most badly code I've ever written

  private def execute = {
    var sourceCode = readProgram

    val input = 5
    var stop = false
    var index = 0
    do {
      val opCode = sourceCode(index)
      val instruction = decodeOpCode(opCode)
      var jump = 0;

      println(
        s"instruction = $instruction   -  opcode = $opCode   -    index = $index"
      )

      instruction.opCode match {
        case 99 =>
          stop = true
        case 1 =>
          val v1 =
            if (instruction.isParam1InPositionMode)
              sourceCode(sourceCode(index + 1))
            else sourceCode(index + 1)
          val v2 =
            if (instruction.isParam2InPositionMode)
              sourceCode(sourceCode(index + 2))
            else sourceCode(index + 2)
          val resultPosition = sourceCode(index + 3)
          val result = v1 + v2
          println(
            s"   instruction: ADD:  v1=$v1 v2=$v2 resultPos=$resultPosition result=$result"
          )
          sourceCode = sourceCode.updated(resultPosition, result)
          println(
            s"              debug pos=$resultPosition value=${sourceCode(resultPosition)}"
          )
        case 2 =>
          val v1 =
            if (instruction.isParam1InPositionMode)
              sourceCode(sourceCode(index + 1))
            else sourceCode(index + 1)
          val v2 =
            if (instruction.isParam2InPositionMode)
              sourceCode(sourceCode(index + 2))
            else sourceCode(index + 2)
          val resultPosition = sourceCode(index + 3)
          val result = v1 * v2
          println(
            s"   instruction: MUL: v1=$v1 v2=$v2 resultPos=$resultPosition result=$result"
          )
          sourceCode = sourceCode.updated(resultPosition, result)
          println(
            s"              debug pos=$resultPosition value=${sourceCode(resultPosition)}"
          )
        case 3 =>
          val v1 =
            if (instruction.isParam1InPositionMode)
              sourceCode(sourceCode(index + 1))
            else sourceCode(index + 1)
          val resultPosition = sourceCode(index + 1)
          println(s"   instruction: STORE: v1=$v1 resultPos=$resultPosition")
          sourceCode = sourceCode.updated(resultPosition, input)
          println(
            s"              debug pos=$resultPosition value=${sourceCode(resultPosition)}"
          )
        case 4 =>
          val v1 =
            if (instruction.isParam1InPositionMode)
              sourceCode(sourceCode(index + 1))
            else sourceCode(index + 1)
          println(s"OUTPUT $v1")
        case 5 =>
          val v1 =
            if (instruction.isParam1InPositionMode)
              sourceCode(sourceCode(index + 1))
            else sourceCode(index + 1)
          val v2 =
            if (instruction.isParam2InPositionMode)
              sourceCode(sourceCode(index + 2))
            else sourceCode(index + 2)
          println(
            s"   instruction: JUMP(if-true): v1=$v1 v2=$v2 --> jumpto $v2"
          )
          jump = if (v1 != 0) v2 else 0
        case 6 =>
          val v1 =
            if (instruction.isParam1InPositionMode)
              sourceCode(sourceCode(index + 1))
            else sourceCode(index + 1)
          val v2 =
            if (instruction.isParam2InPositionMode)
              sourceCode(sourceCode(index + 2))
            else sourceCode(index + 2)
          jump = if (v1 == 0) v2 else 0
          println(
            s"   instruction: JUMP(if-false): v1=$v1 v2=$v2 --> jumpto $jump"
          )
        case 7 =>
          val v1 =
            if (instruction.isParam1InPositionMode)
              sourceCode(sourceCode(index + 1))
            else sourceCode(index + 1)
          val v2 =
            if (instruction.isParam2InPositionMode)
              sourceCode(sourceCode(index + 2))
            else sourceCode(index + 2)
          val resultPosition = sourceCode(index + 3)
          println(
            s"   instruction: LESS-THAN: v1=$v1 v2=$v2 resultPosition=$resultPosition"
          )
          if (v1 < v2) {
            sourceCode = sourceCode.updated(resultPosition, 1)
          } else {
            sourceCode = sourceCode.updated(resultPosition, 0)
          }
        case 8 =>
          val v1 =
            if (instruction.isParam1InPositionMode)
              sourceCode(sourceCode(index + 1))
            else sourceCode(index + 1)
          val v2 =
            if (instruction.isParam2InPositionMode)
              sourceCode(sourceCode(index + 2))
            else sourceCode(index + 2)
          val resultPosition = sourceCode(index + 3)
          println(
            s"   instruction: EQUALS: v1=$v1 v2=$v2 resultPosition=$resultPosition"
          )
          if (v1 == v2) {
            sourceCode = sourceCode.updated(resultPosition, 1)
          } else {
            sourceCode = sourceCode.updated(resultPosition, 0)
          }
        case _ => println("error")
      }

      if (jump != 0) {
        index = jump
      } else {
        index = index + instruction.nextInstructionPointer
      }
    } while (!stop)
  }

  private def decodeOpCode(opCode: Int) = {
    val digits = opCode.toString.map(_.asDigit).toVector.reverse

    val op = if (digits.size <= 1) digits(0) else digits(1) * 10 + digits(0)
    val isParam1InPositionMode =
      if (digits.size < 3 || digits(2) == 0) true else false
    val isParam2InPositionMode =
      if (digits.size < 4 || digits(3) == 0) true else false
    val isParam3InPositionMode =
      if (digits.size < 5 || digits(4) == 0) true else false
    val nextIndex = op match {
      case 1  => 4
      case 2  => 4
      case 3  => 2
      case 4  => 2
      case 5  => 3
      case 6  => 3
      case 7  => 4
      case 8  => 4
      case 99 => 0
    }

    Instruction(
      op,
      isParam1InPositionMode,
      isParam2InPositionMode,
      isParam3InPositionMode,
      nextIndex
    )
  }

  private def readProgram = {
    val lines = FileUtilities.read("day5/input.txt")
    lines.flatMap(line => {
      val numbers = line.split(",").toVector
      numbers.map(_.toInt)
    })
  }

}
