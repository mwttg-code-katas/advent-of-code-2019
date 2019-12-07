package de.advent.of.code.day7

import de.advent.of.code.common.FileUtilities

// -----------------------------
// Lightly refactoring from Day 5
// yet, not functional (and not fully immutable)
// -----------------------------

final case class Instruction(opCode: Int,
                             isParam1InPositionMode: Boolean,
                             isParam2InPositionMode: Boolean,
                             isParam3InPositionMode: Boolean,
                             nextInstructionPointer: Int)

final case class InstructionResult(modifiedSourceCode: Vector[Int], jump: Int, result: Option[Int], inputIndex: Int)

object IntComputer {
  def main(args: Array[String]): Unit = {
    val sourceCode = readProgram
    val result     = execute(sourceCode, Vector(5))
    println(result)
  }

  def execute(inputSourceCode: Vector[Int], input: Vector[Int]): Int = {
    var sourceCode = inputSourceCode
    var result     = 0

    var stop  = false
    var index = 0
    var inputIndex = 0

    do {
      val opCode      = sourceCode(index)
      val instruction = decodeOpCode(opCode)

      val r = instruction.opCode match {
        case 99 =>
          stop = true
          InstructionResult(sourceCode, 0, None, inputIndex)
        case 1 => executeAdd(sourceCode, index, instruction, inputIndex)
        case 2 => executeMul(sourceCode, index, instruction, inputIndex)
        case 3 => executeStoreInput(input, sourceCode, index, inputIndex)
        case 4 => executeOutput(sourceCode, index, instruction, inputIndex)
        case 5 => executeJumpNonZero(sourceCode, index, instruction, inputIndex)
        case 6 => executeJumpEqualsZero(sourceCode, index, instruction, inputIndex)
        case 7 => executeLessThanOperation(sourceCode, index, instruction, inputIndex)
        case 8 => executeEqualsOperation(sourceCode, index, instruction, inputIndex)
      }

      sourceCode = r.modifiedSourceCode
      index      = getNextIndex(index, instruction, r.jump)
      inputIndex = r.inputIndex
      result     = getResult(result, r)

    } while (!stop)

    result
  }

  private def executeEqualsOperation(sourceCode: Vector[Int], index: Int, instruction: Instruction, inputIndex: Int) = {
    val v1             = getValue1(sourceCode, index, instruction)
    val v2             = getValue2(sourceCode, index, instruction)
    val resultPosition = getResultPosition(sourceCode, index)
    val newSourceCode  = equalsOperation(sourceCode, v1, v2, resultPosition)
    InstructionResult(newSourceCode, 0, None, inputIndex)
  }

  private def executeLessThanOperation(sourceCode: Vector[Int], index: Int, instruction: Instruction, inputIndex: Int): InstructionResult = {
    val v1             = getValue1(sourceCode, index, instruction)
    val v2             = getValue2(sourceCode, index, instruction)
    val resultPosition = getResultPosition(sourceCode, index)
    val newSourceCode  = lessThanOperation(sourceCode, v1, v2, resultPosition)
    InstructionResult(newSourceCode, 0, None, inputIndex)
  }

  private def executeJumpEqualsZero(sourceCode: Vector[Int], index: Int, instruction: Instruction, inputIndex: Int): InstructionResult = {
    val v1   = getValue1(sourceCode, index, instruction)
    val v2   = getValue2(sourceCode, index, instruction)
    val jump = getJumpPositionEqualsZero(v1, v2)
    InstructionResult(sourceCode, jump, None, inputIndex)
  }

  private def executeJumpNonZero(sourceCode: Vector[Int], index: Int, instruction: Instruction, inputIndex: Int): InstructionResult = {
    val v1   = getValue1(sourceCode, index, instruction)
    val v2   = getValue2(sourceCode, index, instruction)
    val jump = getJumpPositionNotZero(v1, v2)
    InstructionResult(sourceCode, jump, None, inputIndex)
  }

  private def executeOutput(sourceCode: Vector[Int], index: Int, instruction: Instruction, inputIndex: Int): InstructionResult = {
    val v1 = getValue1(sourceCode, index, instruction)
    InstructionResult(sourceCode, 0, Some(v1), inputIndex)
  }

  private def executeStoreInput(input: Vector[Int], sourceCode: Vector[Int], index: Int, inputIndex: Int): InstructionResult = {
    val resultPosition = sourceCode(index + 1)
    val newSourceCode  = sourceCode.updated(resultPosition, input(inputIndex))
    InstructionResult(newSourceCode, 0, None, inputIndex + 1)
  }

  private def executeMul(sourceCode: Vector[Int], index: Int, instruction: Instruction, inputIndex: Int): InstructionResult = {
    val v1             = getValue1(sourceCode, index, instruction)
    val v2             = getValue2(sourceCode, index, instruction)
    val resultPosition = getResultPosition(sourceCode, index)
    val result         = v1 * v2
    val newSourceCode  = sourceCode.updated(resultPosition, result)
    InstructionResult(newSourceCode, 0, None, inputIndex)
  }

  private def executeAdd(sourceCode: Vector[Int], index: Int, instruction: Instruction, inputIndex: Int): InstructionResult = {
    val v1             = getValue1(sourceCode, index, instruction)
    val v2             = getValue2(sourceCode, index, instruction)
    val resultPosition = getResultPosition(sourceCode, index)
    val result         = v1 + v2
    val newSourceCode  = sourceCode.updated(resultPosition, result)
    InstructionResult(newSourceCode, 0, None, inputIndex)
  }

  private def getResult(result: Int, r: InstructionResult) =
    r.result match {
      case None => result
      case Some(value) => value
    }

  private def equalsOperation(sourceCode: Vector[Int], v1: Int, v2: Int, resultPosition: Int) =
    if (v1 == v2) {
      sourceCode.updated(resultPosition, 1)
    } else {
      sourceCode.updated(resultPosition, 0)
    }

  private def lessThanOperation(sourceCode: Vector[Int], v1: Int, v2: Int, resultPosition: Int) =
    if (v1 < v2) {
      sourceCode.updated(resultPosition, 1)
    } else {
      sourceCode.updated(resultPosition, 0)
    }

  private def getNextIndex(index: Int, instruction: Instruction, jump: Int) =
    if (jump != 0) {
      jump
    } else {
      index + instruction.nextInstructionPointer
    }

  private def getJumpPositionEqualsZero(v1: Int, v2: Int) =
    if (v1 == 0) v2 else 0

  private def getJumpPositionNotZero(v1: Int, v2: Int) =
    if (v1 != 0) v2 else 0

  private def getResultPosition(sourceCode: Vector[Int], index: Int) =
    sourceCode(index + 3)

  private def getValue2(sourceCode: Vector[Int], index: Int, instruction: Instruction) =
    if (instruction.isParam2InPositionMode)
      sourceCode(sourceCode(index + 2))
    else sourceCode(index + 2)

  private def getValue1(sourceCode: Vector[Int], index: Int, instruction: Instruction) =
    if (instruction.isParam1InPositionMode)
      sourceCode(sourceCode(index + 1))
    else sourceCode(index + 1)

  private def decodeOpCode(opCode: Int) = {
    val digits = opCode.toString.map(_.asDigit).toVector.reverse

    val op                     = decodeOperator(digits)
    val isParam1InPositionMode = decodeParam1(digits)
    val isParam2InPositionMode = decodeParam2(digits)
    val isParam3InPositionMode = decodeParam3(digits)
    val instructionLength      = decodeInstructionLength(op)

    Instruction(
      op,
      isParam1InPositionMode,
      isParam2InPositionMode,
      isParam3InPositionMode,
      instructionLength
    )
  }

  private def decodeInstructionLength(op: Int) =
    op match {
      case 1 => 4
      case 2 => 4
      case 3 => 2
      case 4 => 2
      case 5 => 3
      case 6 => 3
      case 7 => 4
      case 8 => 4
      case 99 => 0
    }

  private def decodeParam3(digits: Vector[Int]) =
    if (digits.size < 5 || digits(4) == 0) true else false

  private def decodeParam2(digits: Vector[Int]) =
    if (digits.size < 4 || digits(3) == 0) true else false

  private def decodeParam1(digits: Vector[Int]) =
    if (digits.size < 3 || digits(2) == 0) true else false

  private def decodeOperator(digits: Vector[Int]) =
    if (digits.size <= 1) digits(0) else digits(1) * 10 + digits(0)

  private def readProgram = {
    val lines = FileUtilities.read("day5/input.txt")
    lines.flatMap(line => {
      val numbers = line.split(",").toVector
      numbers.map(_.toInt)
    })
  }
}
