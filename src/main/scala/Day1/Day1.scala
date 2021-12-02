package Day1

import aoc.AoCUtils.parse_input

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App{

  val inputList = parse_input("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day1/input.txt")
  var countIncreases: Int = 0
  var count3Increases: Int = 0
  var lastSum: Int = 0

  @tailrec
  def evaluateDepthIncrease(index:Int):Boolean = {
    if (index >= inputList.length-1) return true
    else {
      if (inputList.apply(index).compareTo(inputList.apply(index + 1)).equals(-1)) {
        countIncreases = countIncreases + 1
      }
      evaluateDepthIncrease(index + 1)
    }
  }

  @tailrec
  def evaluate3DepthIncrease(index:Int):Boolean = {

    if (index >= inputList.length - 1) true
    else {
      val current3Depths = inputList.apply(index) + inputList.apply(index-1) + inputList.apply(index-2)
        if (lastSum < current3Depths){
          count3Increases = count3Increases + 1
        }
      lastSum = current3Depths
      evaluate3DepthIncrease(index + 1)
    }
  }

  evaluateDepthIncrease(0)

  evaluate3DepthIncrease(2)

  println(s"Increases in double depth found: ${countIncreases}")
  println(s"Increases in three measurement depth found: ${count3Increases}")
}