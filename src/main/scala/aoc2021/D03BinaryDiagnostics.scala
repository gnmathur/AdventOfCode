package aoc2021

import utils.AocFileOps
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success}

private object P1Solution {
  import D03BinaryDiagnostics._

  // Derive gamma and epsilon rates from a bit frequency map
  private def getRate(fm: mutable.Map[BitPosition, (ZeroCount, OneCount)], comparison: (ZeroCount, OneCount) => Boolean): Int = {
    // Iterate over the map in bit position order. The map is converted to a list that can be sorted by the key,
    // to enable this iteration. For each bit position, starting from bit position 1, decide whether to choose
    // a bit 0 representation, or a bit 1 representation
    val binary = fm.toSeq.sortBy(x => x._1).map {
      case (a: BitPosition, b: (ZeroCount, OneCount)) => if (comparison(b._1, b._2)) 0 else 1
    }
    Integer.parseInt(binary.mkString(""), 2)
  }

  private def getGammaRate(fm: mutable.Map[BitPosition, (ZeroCount, OneCount)]): Int = {
    getRate(fm, (zc: ZeroCount, oc: OneCount) => zc > oc)
  }

  private def getEpsilonRate(fm: mutable.Map[BitPosition, (ZeroCount, OneCount)]): Int = {
    getRate(fm, (zc: ZeroCount, oc: OneCount) => zc < oc)
  }

  /**
   * Solve the problem
   *
   * @param report A collection with Strings of 5 characters each
   * @return The tuple <gamma, epsilon, power consumption>
   */
  def solve(report: List[String]): (Int, Int, Int) = {
    val fm = getFreqMap(report)
    val ga = getGammaRate(fm)
    val ep = getEpsilonRate(fm)
    (ga, ep, ga * ep)
  }
}

private object P2Solution {
  import D03BinaryDiagnostics._

  // Successively whittle the report list, by examining successive bit positions. Reduce the report till
  // only a single number remains
  private def getRating(report: List[String], compareWithCharFn: (ZeroCount, OneCount) => Char): Int = {

    // Filter out report numbers that fail to match the bit value in <bitPosition> with what's determined by
    // the bit value determined by <compareWithCharFn>
    @tailrec
    def _get02Rating(reducedReport: List[String], bitPosition: BitPosition): Int = {
      if (reducedReport.size > 1) {
        // Determine frequency of zero and one bit in each column position
        val fm = getFreqMap(reducedReport).toSeq.sortBy(_._1)

        _get02Rating(
          reducedReport.filter { s =>
            val bitFreq = fm(bitPosition)
            val bitToCompare = compareWithCharFn(bitFreq._2._1, bitFreq._2._2)
            val bitAtPosition = s.charAt(bitFreq._1)
            (bitAtPosition == bitToCompare)
          },
          bitPosition + 1)
      } else {
        Integer.parseInt(reducedReport.head, 2)
      }
    }
    _get02Rating(report, 0)
  }

  private def getO2Rating(report: List[String]): Int = {
    getRating(report, (zc: ZeroCount, oc: OneCount) => if (oc >= zc) '1' else '0')
  }

  private def getCO2Rating(report: List[String]): Int = {
    getRating(report, (zc: ZeroCount, oc: OneCount) => if (zc <= oc) '0' else '1')
  }

  /**
   * Solve the problem
   *
   * @param report A collection with Strings of 5 characters each
   * @return The tuple <Oxygen generator rating, CO2 scrubber rating, Life support rating>
   */
  def solve(report: List[String]): (BitPosition, BitPosition, BitPosition) = {
    val o2Rating = getO2Rating(report)
    val co2Rating = getCO2Rating(report)
    (o2Rating, co2Rating, o2Rating*co2Rating)
  }
}

object D03BinaryDiagnostics extends App {
  // Types to represent frequency count of a bit in a bit position (column in the report)
  type ZeroCount = Int
  type OneCount = Int
  type BitPosition = Int

  /**
   * Parse the report and create a frequency count for 0 and 1 bits in each or the report columns
   *
   * @param report A collection with a String of N bit values each
   * @return A bit frequency count per column (BitPosition)
   */
  def getFreqMap(report: List[String]): mutable.Map[BitPosition, (ZeroCount, OneCount)] = {
    val bitCounterMap = mutable.Map[BitPosition, (ZeroCount, OneCount)]()

    def updateMapForBitPosition(ch: Char, bp: BitPosition) = {
      val mapTuple: (ZeroCount, OneCount) = bitCounterMap.getOrElse(bp, (0: ZeroCount, 0: OneCount))
      if (ch == '0') bitCounterMap += bp -> (mapTuple._1 + 1, mapTuple._2)
      else bitCounterMap += bp -> (mapTuple._1, mapTuple._2 + 1)
    }

    report.map {
      num =>
        num.zipWithIndex.map {
          case (ch, chIndex) => updateMapForBitPosition(ch, chIndex)
        }
    }
    bitCounterMap
  }

  // - Test case for part 1 -
  AocFileOps
    .readInputAsStringList("src/main/resources/D03TestInput.txt") match {
    case Success(testVector) => {
      val (ga, ep, powerc) = P1Solution.solve(testVector)
      assert(22 == ga)
      assert(9 == ep)
      assert(198 == powerc)
    }
    case Failure(exception) => println("error parsing test input")
  }

  // - Solution for part 1-
  AocFileOps
    .readInputAsStringList("src/main/resources/D03Input.txt") match {
    case Success(testVector) => {
      val (ga, ep, powerc) = P1Solution.solve(testVector)
      assert(1616 == ga)
      assert(2479 == ep)
      assert(4006064 == powerc)
    }
    case Failure(exception) => println("error parsing test input")
  }

  // -- Test case for part 2
  AocFileOps
    .readInputAsStringList("src/main/resources/D03TestInput.txt") match {
    case Success(testVector) => {
      val (o2rating, co2Rating, lifeSupportRating) = P2Solution.solve(testVector)
      assert(23 == o2rating)
      assert(10 == co2Rating)
      assert(230 == lifeSupportRating)
    }
    case Failure(exception) => println("error parsing test input")
  }

  // -- Solution for part2
  AocFileOps
    .readInputAsStringList("src/main/resources/D03Input.txt") match {
    case Success(testVector) => {
      val (o2rating, co2Rating, lifeSupportRating) = P2Solution.solve(testVector)
      assert(1599 == o2rating)
      assert(3716 == co2Rating)
      assert(5941884 == lifeSupportRating)
    }
    case Failure(exception) => println("error parsing test input")
  }
}
