package aoc2021

import aoc2021.utils.FileOperations

import scala.collection.mutable
import scala.util.{Failure, Success}

object D03BinaryDiagnostics extends App {
  // Types to represent frequency count of a bit in a bit position (column in the report)
  type ZeroCount = Int
  type OneCount = Int

  // A type to represent a bit position in a report line
  sealed case class BitPosition(pos: Int)
  implicit object BitPositionOrdering extends Ordering[BitPosition] {
    override def compare(x: BitPosition, y: BitPosition): Int = x.pos - y.pos
  }
  val BP1 = BitPosition(1)
  val BP2 = BitPosition(2)
  val BP3 = BitPosition(3)
  val BP4 = BitPosition(4)
  val BP5 = BitPosition(5)
  val BP6 = BitPosition(6)
  val BP7 = BitPosition(7)
  val BP8 = BitPosition(8)
  val BP9 = BitPosition(9)
  val BP10 = BitPosition(10)
  val BP11 = BitPosition(11)
  val BP12 = BitPosition(12)

  /**
   * Parse the report and create a frequency count for 0 and 1 bits in each or the report columns
   *
   * @param report A collection with a String of N bit values each
   * @return A bit frequency count per column (BitPosition)
   */
  private def getFreqMap(report: List[String])= {
    val bitCounterMap = mutable.Map[BitPosition, (ZeroCount, OneCount)]()

    def updateMapForBitPosition(ch: Char, bp: BitPosition) = {
      val mapTuple: (ZeroCount, OneCount) = bitCounterMap.getOrElse(bp, (0: ZeroCount, 0: OneCount))
      if (ch == '0') bitCounterMap += bp -> (mapTuple._1 + 1, mapTuple._2)
      else bitCounterMap += bp -> (mapTuple._1, mapTuple._2 + 1)
    }

    report.map {
      num =>
        num.zipWithIndex.map {
          case (ch, chIndex) => chIndex match {
            case 0 => updateMapForBitPosition(ch, BP1)
            case 1 => updateMapForBitPosition(ch, BP2)
            case 2 => updateMapForBitPosition(ch, BP3)
            case 3 => updateMapForBitPosition(ch, BP4)
            case 4 => updateMapForBitPosition(ch, BP5)
            case 5 => updateMapForBitPosition(ch, BP6)
            case 6 => updateMapForBitPosition(ch, BP7)
            case 7 => updateMapForBitPosition(ch, BP8)
            case 8 => updateMapForBitPosition(ch, BP9)
            case 9 => updateMapForBitPosition(ch, BP10)
            case 10 => updateMapForBitPosition(ch, BP11)
            case 11 => updateMapForBitPosition(ch, BP12)
          }
        }
    }
    bitCounterMap
  }

  // Derive gamma and epsilon rates from a bit frequency map
  def getRate(fm: mutable.Map[BitPosition, (ZeroCount, OneCount)], comparison: (ZeroCount, OneCount) => Boolean) = {
    // Iterate over the map in bit position order. The map is converted to a list that can be sorted by the key,
    // to enable this iteration. For each bit position, starting from bit position 1, decide whether to choose
    // a bit 0 representation, or a bit 1 representation
    val binary = fm.toSeq.sortBy(x => x._1).map {
      case (a: BitPosition, b: (ZeroCount, OneCount)) => if (comparison(b._1, b._2)) 0 else 1
    }
    Integer.parseInt(binary.mkString(""), 2 )
  }
  def getGammaRate(fm: mutable.Map[BitPosition, (ZeroCount, OneCount)]) = {
    getRate(fm, (zc: ZeroCount, oc: OneCount) => zc > oc)
  }
  def getEpsilonRate(fm: mutable.Map[BitPosition, (ZeroCount, OneCount)]) = {
    getRate(fm, (zc: ZeroCount, oc: OneCount) => zc < oc)
  }

  /**
   * Solve the problem
   *
   * @param report A collection with Strings of 5 characters each
   * @return The tuple <gamma, epsilon, power consumption>
   */
  def solve(report: List[String]) = {
    val fm = getFreqMap(report)
    val ga = getGammaRate(fm)
    val ep = getEpsilonRate(fm)
    (ga, ep, ga*ep)
  }

  // - Test Case -
  FileOperations
    .readVectorFromFile("src/main/resources/D03P1TestInput.txt") match {
    case Success(testVector) => {
      val (ga, ep, powerc) = solve(testVector)
      assert(22 == ga)
      assert(9 == ep)
      assert(198 == powerc)
    }
    case Failure(exception) => println("error parsing test input")
  }

  // - Problem Solution -
  FileOperations
    .readVectorFromFile("src/main/resources/D03P1Input.txt") match {
    case Success(testVector) => {
      val (ga, ep, powerc) = solve(testVector)
      assert(1616 == ga)
      assert(2479 == ep)
      assert(4006064 == powerc)
    }
    case Failure(exception) => println("error parsing test input")
  }
}
