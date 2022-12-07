package aoc2020

import utils.AocFileOps

import scala.util.{Failure, Success}

object D02PasswordPhilosophy extends App {
  case class LeftNum(n: Int) extends AnyVal
  case class RightNum(n: Int) extends AnyVal
  case class Letter(l: Char) extends AnyVal
  case class Password(passString: String) extends AnyVal

  object Password {
    def isValid(p: Policy): Boolean =
      (p.pass.passString(p.l.n - 1) == p.letter.l &&
        p.pass.passString(p.r.n-1) != p.letter.l) ||
        (p.pass.passString(p.l.n - 1) != p.letter.l &&
          p.pass.passString(p.r.n - 1) == p.letter.l)

  }

  case class Policy(l: LeftNum, r: RightNum, letter: Letter, pass: Password)
  object Policy {
    def make(s: String): Policy = {
      val tokens = s.split(" ")
      val range = tokens(0).split("-").map(_.toInt)
      val leftNum = LeftNum(range(0))
      val rightNum = RightNum(range(1))
      val letter = Letter(tokens(1).dropRight(1)(0))

      Policy(leftNum, rightNum, letter, Password(tokens(2)))
    }
  }

  def parse(input: List[String]): List[Policy] = {
    input.map(Policy.make)
  }

  def solvePart1(input: List[String]): Long = parse(input).count(Password.isValid)

  val input = List(
    "1-3 a: abcde", // valid
    "1-3 b: cdefg", // invalid
    "2-9 c: ccccccccc") // invalid

  assert(1 == solvePart1(input))


  // Solve for part 1 and 2
  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2020/2020D02Input.lst") match {
    case Success(input) => assert(441 == solvePart1(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  val rrr = "11-3 a: aabcder".split("(\\d+)-(\\d+) (\\w): (\\w+)")
  println(rrr)
}
