package Day7

import aoc.AoCUtils.parse_comma_string_for_ints

import scala.language.postfixOps

object Day7 extends App {

  val inputList = parse_comma_string_for_ints("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day7/input.txt").toSeq

  println(solution(inputList,true))



  private def solution(crabs: Seq[Int], part2: Boolean): Int = (0 to crabs.max).foldLeft(Int.MaxValue) { (acc, n) =>
    val fuelUsed = crabs.foldLeft(0) {
      case (fuelAcc, crab) if part2 =>
        val moves    = (crab - n).abs
        val fuelUsed = (math.pow(moves, 2).toInt + moves) / 2 // Gauss formula: https://nrich.maths.org/2478
        fuelAcc + fuelUsed
      case (fuelAcc, crab) => fuelAcc + (crab - n).abs
    }
    if (fuelUsed < acc) fuelUsed else acc
  }
}

