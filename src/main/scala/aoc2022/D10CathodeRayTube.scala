package aoc2022

import utils.AocFileOps

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object D10CathodeRayTube extends App {
  trait Instruction {
    def operand: Option[Int]
  }
  case class Add(operand: Option[Int]) extends Instruction
  case class Noop(operand:Option[Int]) extends Instruction

  // Register value
  case class Register(v: Long) extends AnyVal
  // The cycle the current instruction is on
  case class InstructionCycles(v: Int) extends AnyVal
  // Accumulate signal strength values
  type SignalStrength = Map[Int, Long]

  def lightUp(cycle: Int, xPos: Long) = {
    if (cycle >= xPos - 1 && cycle <= xPos + 1) true else false
  }

  @tailrec
  def doCycle(
               program: List[Instruction],
               ic: InstructionCycles,
               register: Register,
               cycleNum: Int,
               sigStrength: SignalStrength,
               fb: Vector[Char]
             ): (SignalStrength, Vector[Char]) =
    if (program.isEmpty) (sigStrength, fb) else {
      val newFb = if (lightUp((cycleNum-1)%40, register.v)) fb :+ '#' else fb :+ '.'
      val newCycle = cycleNum + 1
      val newSS = if (cycleNum % 20 == 0) sigStrength + (cycleNum -> cycleNum * register.v) else sigStrength

      program.head match {
        case Add(operand) =>
          val newProgram = if (ic.v == 2) program.tail else program
          val newIc = if (ic.v == 2) InstructionCycles(1) else InstructionCycles(ic.v +1 )
          val newRegister = if (ic.v == 2) Register(register.v + operand.get) else register
          doCycle(newProgram, newIc, newRegister, newCycle, newSS, newFb)

        case Noop(_) =>
          val newProgram = if (ic.v == 1) program.tail else program
          val newIc = InstructionCycles(1)
          val newRegister = register
          doCycle(newProgram, newIc, newRegister, newCycle, newSS, newFb)
      }
    }

  val addRegex = """addx (-?[0-9]+)""".r
  val noopRegex = """noop""".r

  def parse(input: List[String]): List[Instruction] = input.map {
      case addRegex(operand) => Add(Some(operand.toInt))
      case noopRegex => Noop(None)
  }

  def drawFb(fb: Vector[Char]): Unit = {
    fb.zipWithIndex.foreach { case (pixel, idx) =>
      if (idx % 40 == 0) {
        println("\n" + pixel)
      } else {
        print(pixel)
      }
    }
  }

  def solvePart1(input: List[String]) = {
    val (ss, fb) = doCycle(parse(input), InstructionCycles(1), Register(1), 1, Map(), Vector())
    drawFb(fb)
    ss(20) + ss(60) + ss(100) + ss(140) + ss(180) + ss(220)
  }

  // Test cases
  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D10TestInput.lst") match {
    case Success(input) => assert(13140 == solvePart1(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  // Solve Part 1
  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D10Input.lst") match {
    case Success(input) => assert(14620 == solvePart1(input))
    // Frame buffer draws BJFRHRFU
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

}
