package aoc2021

import java.io.BufferedReader
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.StreamHasToScala

object D06LanternFish extends App {
  type TimerValue = Int
  type Count = Long

  /**
   * The naive approach does not scale as there is an exponential increase in Lanternfish as the days increase. This
   * approach uses a histogram instead - it will keep a map of Lanternfish at specific Timers values from
   * 0 to 8, instead of tracking the lifecycle of each as in the naive approach
   */
  def solveHistogram(input: List[Int], iterations: Int) = {
    // A map from timer values to a count of how many Lanternfish are at the timer value
    val initHistogram = Map[TimerValue, Count](
      0 -> 0L, 1 -> 0L, 2 -> 0L, 3 -> 0L, 4 -> 0L, 5 -> 0L, 6 -> 0L, 7 -> 0L, 8 -> 0L
    )
    val inputHistogram: Map[TimerValue, Count] = input.groupBy(identity).map { case (n, nList) =>
      (n, nList.size.toLong)
    }
    // Set the initial histogram value from the input
    val h = initHistogram ++ inputHistogram
    // For each day, update the histogram
    (1 to iterations).foldLeft(h) { case (acc, _) =>
      // Count of Lanternfish at timer 0
      val C = acc(0)
      // Inspect Lanternfish at each timer value and advance their timers
      // Set the number of Lanternfish at age 8 as 0
      val a = acc.map { case (timerValue, count) =>
        if (timerValue < 8) (timerValue -> acc(timerValue + 1))
        else (timerValue -> 0L)
      }
      // Account for new Lanternfish starting at age 8 and reset timer of ones which reset to 0
      // c Lanternfish
      // a. start at age 8
      // b. add to the count of Lanterfish at age 6
      val r = (a + (8 -> C)) + (6 -> (a(6) + C))
      r
    }.foldLeft(0L) { case (acc, key) => acc + key._2} // count all the frequency values
  }

  def solveNaive(input: List[Int], iterations: Int): List[Int] = {
    (1 to iterations).foldLeft(input) { (acc, a) =>
      // Add new lantern fish based on how many now exist with their timer at 0
      val rl = acc ++ List.fill(acc.count(_ == 0))(9)
      // Reset timer if it has reached 0, or reduce it by one otherwise
      rl.map { ele => if (ele == 0) 6 else (ele-1) }
    }
  }

  def run(filePath: String, iterations: Int) = {
    val f: BufferedReader = Files.newBufferedReader(Paths.get(filePath), Charset.forName("UTF-8"))
    val lines: List[String] = f.lines().toScala(LazyList).toList
    val internalTimers: Array[Int] = lines.head.split(",") map Integer.parseInt
    solveHistogram(internalTimers.toList, iterations)
  }

  // Test case for part 1
  assert(5934 == run("src/main/resources/D06TestInput.txt", 80))
  // Problem solution for part 1
  assert(356190 == run("src/main/resources/D06Input.txt", 80))

  // Test case for part 2
  assert(26984457539L == run("src/main/resources/D06TestInput.txt", 256))
  // Problem  solution for part 2
  assert(1617359101538L == run("src/main/resources/D06Input.txt", 256))

}
