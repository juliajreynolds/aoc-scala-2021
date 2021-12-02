package Day2

import aoc.AoCUtils.parse_string_input

import scala.annotation.tailrec

object Day2  extends App{

  val inputList = parse_string_input("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day2/input.txt")

  var horizontal: Int = 0
  var depth: Int = 0
  var aim: Int = 0

  @tailrec
  def calculatePosition(index:Int):Boolean = {
    if (index >= inputList.length) return true
    else {
      inputList.apply(index).split(' ')
      inputList.apply(index).split(' ').apply(0) match {
        case "up" => depth = depth - inputList.apply(index).split(' ').apply(1).toInt
        case "down" => depth = depth + inputList.apply(index).split(' ').apply(1).toInt
        case "forward" => horizontal = horizontal + inputList.apply(index).split(' ').apply(1).toInt
      }
      calculatePosition(index + 1)

    }
  }
  def goDown(amount:Int): Unit ={
    aim = aim + amount
  }
  def goForward(amount:Int): Unit ={
    horizontal = horizontal + amount
    depth = depth + (aim * amount)
  }


  def calculatePosition2(index:Int):Boolean = {
    if (index >= inputList.length) return true
    else {
      inputList.apply(index).split(' ')
      inputList.apply(index).split(' ').apply(0) match {
        case "up" => aim = aim - inputList.apply(index).split(' ').apply(1).toInt
        case "down" => goDown(inputList.apply(index).split(' ').apply(1).toInt)
        case "forward" => goForward(inputList.apply(index).split(' ').apply(1).toInt)
      }
      calculatePosition2(index + 1)
    }
  }
  calculatePosition2(0)
  println(s"horizontal ${horizontal} times depth ${depth} equals " + (horizontal * depth))

}
