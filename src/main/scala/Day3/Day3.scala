package Day3

import Day2.Day2.{depth, horizontal, inputList}
import aoc.AoCUtils.parse_string_input

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable

object Day3 extends App {

  val inputList = parse_string_input("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day3/testinput.txt")

  var oneCount: Int = 0
  var zeroCount: Int = 0

  val indexBitCounterList = initPositionBitCounters()

  var gammaRate: String = ""
  var epsilonRate: String = ""

  def zeroCounters() = {
    zeroCount = 0
    oneCount = 0
  }


  def parseCode(index: Int, code: String): Boolean = {
    if (index >= (code.length)) true
    else {
      code.toCharArray.apply(index) match {
        case '0' =>  indexBitCounterList.apply(index).incrementZero()
        case '1' => indexBitCounterList.apply(index).incrementOne()
      }
      parseCode(index + 1, code)
    }

  }


  def writeGammaAndEpsilonBits(): Boolean = {
    var index:Int = 0
    while( index < indexBitCounterList.size ){
      if(indexBitCounterList.apply(index).zeroCount == indexBitCounterList.apply(index).oneCount) println("equal bit count cannot write value")
      else if (indexBitCounterList.apply(index).zeroCount > indexBitCounterList.apply(index).oneCount) {
        gammaRate = gammaRate + "0"
        epsilonRate = epsilonRate + "1"
      }else {
        gammaRate = gammaRate + "1"
        epsilonRate = epsilonRate + "0"
      }
      index = index + 1
    }

    true
  }

  @tailrec
  def calculatePowerConsumption(index: Int): Boolean = {

    if (index >= inputList.length) return true
    else {
      parseCode(0, inputList.apply(index))

    }

    calculatePowerConsumption(index + 1)

  }

  def initPositionBitCounters():mutable.Map[Int, BitCount] = {
    var bitCountMap = mutable.Map[Int,BitCount]()
    var index:Int = 0
    while( index < inputList.apply(0).length ){
      bitCountMap(index) = new BitCount(0,0)
      index = index + 1
    }
    bitCountMap
  }

  initPositionBitCounters()
  calculatePowerConsumption(0)
  writeGammaAndEpsilonBits()
  println("gammaRate: " + gammaRate)
  println("epsilonRate: " + epsilonRate)


//build binary string
//convert to int val decimalValue: Int = Integer.parseInt(c, 2)
}

class BitCount(var zeroCount:Int, var oneCount:Int){
  def incrementOne():Boolean ={
    oneCount = oneCount + 1
    true
  }
  
  def incrementZero():Boolean = {
    zeroCount = zeroCount + 1
    true
  }
}
