package aoc2021

import java.io.BufferedReader
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters.StreamHasToScala

private object P1 {
  import D04GiantSquid._

  def solve(b: Boards, drawnNumbers: Array[Int]): Int = {
    var boards = b
    for (n <- drawnNumbers) {
      boards = boards.map { board => markDrawnNumber(board, n) }
      for (board <- boards) {
      checkForWinAndCompute(board) match {
          case Some(v) => return v*n
          case _ =>
        }
      }
    }
    -1
  }
}

private object P2 {
  import D04GiantSquid._

  def solve(b: Boards, drawnNumbers: Array[Int]): Int = {
    var mb: Boards = b
    val solvedOrder = ListBuffer[Board]()
    var lastSolvedNumber: Option[Int] = None
    for (n <- drawnNumbers) {
      mb = mb.map { board => markDrawnNumber(board, n) }
      for (board <- mb) {
        if (checkBoardForWin(board).isDefined) {
          solvedOrder += board
          mb = mb.filter((b: Board) => !b.eq(board))
          lastSolvedNumber = Some(n)
        }
      }
    }
    val x = solvedOrder.toList.reverse.head
    compute(x) * lastSolvedNumber.get
  }
}

object D04GiantSquid extends App {
  case class Cell(num: Int, mark: Boolean = false)
  type Board = Array[Array[Cell]]
  type Boards = List[Board]

  @tailrec
  def buildBoards(boards: Boards, inputLines: Iterator[String]): Boards = {
    if (inputLines.hasNext) {
      inputLines.next()
      val board = inputLines.take(5)
        .toArray.map(_.strip()
        .split("\\W+")
        .map(x => Cell(x.toInt, mark = false)))
      buildBoards(boards :+ board, inputLines)
    } else {
      boards
    }
  }

  // Check each cell on the board and mark a number on the board as drawn if it matches the given number
  def markDrawnNumber(b: Board, n: Int): Board = {
    b.map { numberCellsInRow =>
      numberCellsInRow.map { cell =>
        if (cell.num == n) cell.copy(mark = true)
        else cell
      }
    }
  }

  // Check if the given sequence of cells are all marked as drawn
  def checkRowOrColumn(cells: Array[Cell]): Boolean = (cells.count(_.mark) == 5)
  // Check board row by row and see if a row matches has all drawn entries, indicating a win
  def checkBoardByRow(b: Board): Option[Board] = b.collectFirst { case ele if checkRowOrColumn(ele) => b }
  // Check if there is a winning row of column, and return the board if yes
  def checkBoardForWin(b: Board): Option[Board] = checkBoardByRow(b).orElse(checkBoardByRow(b.transpose).orElse(None))

  // Compute the solution for a board with a winning row or column
  def compute(b: Board): Int = {
    val r: Array[Cell] = for {
      r <- b
      c <- r
      if !c.mark
    } yield (c)
    // Add numbers in all unmarked cells
    r.foldLeft(0)((acc, cell) => acc + cell.num)
  }

  // Check row or column for a win.
  def checkForWinAndCompute(b: Board): Option[Int] = {
    checkBoardForWin(b).map(board => compute(board))
  }

  def run(filePath: String, golden: Int, solve: (Boards, Array[Int]) => Int) = {
    val f: BufferedReader = Files.newBufferedReader(Paths.get(filePath), Charset.forName("UTF-8"))
    val fIterator: Iterator[String] = f.lines().toScala(Iterator)
    val drawnNumbers: Array[Int] = fIterator.next().split(",").map(_.toInt)
    val boards: Boards = buildBoards(List[Board](), fIterator)
    assert(golden == solve(boards, drawnNumbers))
  }

  run(filePath = "src/main/resources/D04TestInput.txt", 4512, P1.solve)
  run(filePath = "src/main/resources/D04Input.txt", 65325, P1.solve)

  run(filePath = "src/main/resources/D04TestInput.txt", 1924, P2.solve)
  run(filePath = "src/main/resources/D04Input.txt", 4624, P2.solve)
}
