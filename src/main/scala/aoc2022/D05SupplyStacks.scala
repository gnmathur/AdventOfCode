package aoc2022

import utils.AocFileOps

import scala.collection.mutable
import scala.util.{Failure, Success}

object D05SupplyStacks extends App {
  case class Move(num: Int, from: Int, to: Int)
  object Move {
    def apply(line: String): Move = {
      val moveInfo = line.split(" ")
      Move(moveInfo(1).toInt, moveInfo(3).toInt, moveInfo(5).toInt)
    }
  }
  type CrateStacksWithIdent = Map[Int, mutable.Stack[Char]]
  type Moves = Array[Move]

  def parse(input: String): (Int, CrateStacksWithIdent, Moves) = {
    val cratesAndMoves: Array[String] = input.split("\n\n")

    val crates = cratesAndMoves(0).split("\n")
    val max = crates.last.split("\\W+").last.toInt

    val crateStacksWithIdent = (1 to (max))
      .map(_ -> mutable.Stack[Char]()) // indexed sequence
      .toMap // tuple sequence to Map
    val topToBottomHorizontalCrateView = crates.dropRight(1)
      .map(_.grouped(4)
        .map(_(1)).toSeq)
    topToBottomHorizontalCrateView
      .reverse // Bottom to top horizontal crate view
      .map { _.zipWithIndex.filter { case (ch, _) => ch != ' ' }
        .map { case (crate, crateStackIdent) => crateStacksWithIdent(crateStackIdent+1).push(crate) }
      }
    val moves: Array[Move] = cratesAndMoves(1).split("\n").map(Move(_))
    (max, crateStacksWithIdent, moves)
  }

  def solvePart2(input: String): String = {
    val cm = parse(input)
    cm._3.foreach { move =>
      val crateStacksWithIdent = cm._2
      val cratesToMove = (1 to move.num).foldLeft("") { (acc, i) =>
        val fromStack = crateStacksWithIdent(move.from)
        acc + fromStack.pop()
      }
      cratesToMove.reverse.foreach { crate => crateStacksWithIdent(move.to).push(crate) }
    }
    (1 to cm._1).map(cm._2(_).top).mkString("")
  }

  def solvePart1(input: String): String = {
    val cm = parse(input)
    cm._3.foreach { move =>
      val crateStacksWithIdent = cm._2
      (1 to move.num).foreach { i =>
        val fromStack = crateStacksWithIdent(move.from)
        val toStack = crateStacksWithIdent(move.to)
        toStack.push(fromStack.pop())
      }
    }
    (1 to cm._1).map(cm._2(_).top).mkString("")
  }

  AocFileOps
    .readInputAsString("src/main/resources/aoc2022/2022D05TestInput.lst") match {
    case Success(input) =>
      assert("CMZ" == solvePart1(input))
      assert("MCD" == solvePart2(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  AocFileOps
    .readInputAsString("src/main/resources/aoc2022/2022D05Input.lst") match {
    case Success(input) =>
      assert("TBVFVDZPN" == solvePart1(input))
      assert("VLCWHTDSZ" == solvePart2(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
