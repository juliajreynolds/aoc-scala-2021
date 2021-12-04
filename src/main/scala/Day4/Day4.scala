package Day4

import aoc.AoCUtils.parse_string_input

import scala.annotation.tailrec
import scala.collection.mutable

object Day4 extends App {

  val inputList = parse_string_input("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day3/input.txt")

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
        case '0' => indexBitCounterList.apply(index).incrementZero()
        case '1' => indexBitCounterList.apply(index).incrementOne()
      }
      parseCode(index + 1, code)
    }

  }

  def countBitsAtIndexForList(characterIndex: Int, filterList: List[String]): BitCount = {
    var index: Int = 0
    val bitCount: BitCount = new BitCount(0, 0)
    while (index < filterList.size) {
      filterList.apply(index).toCharArray.apply(characterIndex) match {
        case '0' => bitCount.incrementZero()
        case '1' => bitCount.incrementOne()
      }
      index = index + 1
    }
    bitCount
  }

  def writeGammaAndEpsilonBits(): Boolean = {
    var index: Int = 0
    while (index < indexBitCounterList.size) {
      if (indexBitCounterList.apply(index).zeroCount == indexBitCounterList.apply(index).oneCount) println("equal bit count cannot write value")
      else if (indexBitCounterList.apply(index).zeroCount > indexBitCounterList.apply(index).oneCount) {
        gammaRate = gammaRate + "0"
        epsilonRate = epsilonRate + "1"
      } else {
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

  def initPositionBitCounters(): mutable.Map[Int, BitCount] = {
    var bitCountMap = mutable.Map[Int, BitCount]()
    var index: Int = 0
    while (index < inputList.apply(0).length) {
      bitCountMap(index) = new BitCount(0, 0)
      index = index + 1
    }
    bitCountMap
  }


  def filterForOxyGenRating(index: Int, filterList: List[String]): String = {
    var filteredOxyGen: List[String] = List()
    //already know the most popular in the starting list index 0
    filteredOxyGen = filterList.filter(_.charAt(0) == indexBitCounterList.apply(0).mostCommonBit())
    var localIndex = 1
    if (filteredOxyGen.size == 1) println(filteredOxyGen.apply(0))
    else {
      while (localIndex < filterList.apply(0).length) {
        val bitCount: BitCount = countBitsAtIndexForList(localIndex, filteredOxyGen)
        filteredOxyGen = filteredOxyGen.filter(_.charAt(localIndex) == bitCount.mostCommonBit())
        localIndex = localIndex + 1
      }
    }
    filteredOxyGen.head
  }

  def filterForC02ScrubberRating(index: Int, filterList: List[String]): String = {
    var filteredC02Scrubber: List[String] = List()
    filteredC02Scrubber = filterList.filter(_.charAt(0) != indexBitCounterList.apply(0).mostCommonBit())
    var localIndex = 1
    if (filteredC02Scrubber.size == 1) println(filteredC02Scrubber.apply(0))
    else {
      while (localIndex < filterList.apply(0).length && filteredC02Scrubber.size>1) {
        val bitCount: BitCount = countBitsAtIndexForList(localIndex, filteredC02Scrubber)
        filteredC02Scrubber = filteredC02Scrubber.filter(_.charAt(localIndex) != bitCount.mostCommonBit())
        localIndex = localIndex + 1
      }
    }
    filteredC02Scrubber.head
  }

  initPositionBitCounters()
  calculatePowerConsumption(0)
  //writeGammaAndEpsilonBits()
  //println("gammaRate: " + gammaRate)
  //println("epsilonRate: " + epsilonRate)
//  println("filterForOxyGenRating " + filterForOxyGenRating(0, inputList))
//  println("filterForC02ScrubberRating " + filterForC02ScrubberRating(0, inputList))

  // println("oxyGenList : " + oxyGenList)

  //println("power consumption : " + (Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2)))
  val oxy = Integer.parseInt(filterForOxyGenRating(0, inputList), 2)
  val co2 = Integer.parseInt(filterForC02ScrubberRating(0, inputList), 2)
  println("life support rating : " + (oxy * co2) )

}

class BitCount(var zeroCount: Int, var oneCount: Int) {
  def incrementOne(): Boolean = {
    oneCount = oneCount + 1
    true
  }

  def incrementZero(): Boolean = {
    zeroCount = zeroCount + 1
    true
  }

  def mostCommonBit(): Char = {
    if (oneCount >= zeroCount) '1'
    else if (oneCount < zeroCount) '0'
    else '1'
  }
}
