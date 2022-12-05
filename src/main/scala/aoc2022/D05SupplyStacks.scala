package aoc2022

import jdk.javadoc.internal.doclets.toolkit.util.DocFinder.Input
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
  type Crates = Map[Int, mutable.Stack[Char]]
  type Moves = Array[Move]

  def parse(input: String): (Int, Crates, Moves) = {
    val cratesAndMoves: Array[String] = input.split("\n\n")

    val crates = cratesAndMoves(0).split("\n")
    val max = crates.last.split("\\W+").last.toInt

    val cc = crates.dropRight(1).map((l: String) => l.grouped(4).map(q => q(1)).toSeq)
    val cMap = (1 to(max)).map(i => i -> mutable.Stack[Char]()).toMap
    cc.reverse.map(chsq => chsq.zipWithIndex.filter {case (ch, _) => ch != ' ' }.map { case (cg, k) => cMap(k+1).push(cg) } )
    val moves: Array[Move] = cratesAndMoves(1).split("\n").map(Move(_))
    (max, cMap, moves)
  }

  def solvePart1(input: String): String = {
    val cm = parse(input)
    cm._3.foreach { move =>
      val x: Crates = cm._2
      (1 to move.num).foreach { i =>
        val fromStack = x(move.from)
        val toStack = x(move.to)
        toStack.push(fromStack.pop())
      }
    }
    (1 to cm._1).map(i => cm._2(i).top).mkString("")
  }

  AocFileOps
    .readInputAsString("src/main/resources/aoc2022/2022D05TestInput.lst") match {
    case Success(input) => println(solvePart1(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  AocFileOps
    .readInputAsString("src/main/resources/aoc2022/2022D05Input.lst") match {
    case Success(input) => println(solvePart1(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
