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

  case class Register(v: Long)
  case class InstructionCycles(v: Int) extends AnyVal
  type SignalStrength = Map[Int, Long]

  @tailrec
  def doCycle(program: List[Instruction], ic: InstructionCycles, r: Register, cycleNum: Int, ss: SignalStrength): SignalStrength =
    if (program.isEmpty) ss else {
      val newCycle = cycleNum + 1
      val newSS = if (cycleNum % 20 == 0) ss + (cycleNum -> cycleNum * r.v) else ss

      program.head match {
        case Add(operand) =>
          val newProgram = if (ic.v == 2) program.tail else program
          val newIc = if (ic.v == 2) InstructionCycles(1) else InstructionCycles(ic.v +1 )
          val newRegister = if (ic.v == 2) Register(r.v + operand.get) else r
          doCycle(newProgram, newIc, newRegister, newCycle, newSS)
        case Noop(_) =>
          val newProgram = if (ic.v == 1) program.tail else program
          val newIc = InstructionCycles(1)
          val newRegister = r
          doCycle(newProgram, newIc, newRegister, newCycle, newSS)
      }
    }

  val addRegex = """addx (-?[0-9]+)""".r
  val noopRegex = """noop""".r

  def parse(input: List[String]): List[Instruction] = input.map {
      case addRegex(operand) => Add(Some(operand.toInt))
      case noopRegex => Noop(None)
  }

  def solvePart1(input: List[String]) = {
    val ss = doCycle(parse(input), InstructionCycles(1), Register(1), 1, Map())
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
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
