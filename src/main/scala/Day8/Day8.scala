package Day8

import aoc.AoCUtils.parse_string_input

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps

object Day8 extends App {

  val inputList = parse_string_input("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day8/input.txt")
  val knownDigitCountMap = new mutable.HashMap[Int, Int]()
  val FIVE = "cdfbe"
  val OTHERFIVE = "cdfeb"
  val OTHERFIVE2 = "dcbef"
  val OTHERFIVE3 = "cbgef"
  val OTHERFIVE4 = "bagce"
  val TWO = "cdgba"
  val OTHERTWO = "gcdfa"
  val THREE = "fbcad"
  val OTHERTHREE = "fcadb"
  val OTHERTHREE2 = "cdbaf"
  val OTHERTHREE3 = "cefdb"
  val OTHERTHREE4 = "bfgea"
  val OTHERTHREE5 = "cfgab"
  val NINE = "cefabd"
  val OTHERNINE = "cefbgd"
  val OTHERNINE2 = "fcgedb"
  val OTHERNINE3 = "fdcagb"
  val OTHERNINE4 = "efabcd"
  val OTHERNINE5 = "cdeabf"
  val SIX = "cdfgeb"
  val OTHERSIX = "bcgafe"
  val ZERO = "cagedb"
  val OTHERZERO = "cdebag"
  val OTHERZERO1 = "cedbag"

  var OUTPUTSUM: Long = 0L


  def initializeKnownDigitCountMap(): Boolean = {
    knownDigitCountMap.put(1, 0)
    knownDigitCountMap.put(2, 0)
    knownDigitCountMap.put(3, 0)
    knownDigitCountMap.put(4, 0)
    knownDigitCountMap.put(5, 0)
    knownDigitCountMap.put(6, 0)
    knownDigitCountMap.put(7, 0)
    knownDigitCountMap.put(8, 0)
    knownDigitCountMap.put(9, 0)
    true
  }

  def isAMatch(s1: String, s2: String): Boolean = {
    s1 == s2
  }

  def decipherValue(code: String): String = {
    var result:String = ""
    if (isAMatch(code, ZERO) || isAMatch(code, OTHERZERO)) result = 0.toString
    else if (isAMatch(code, TWO) || isAMatch(code, OTHERTWO)) result = 2.toString
    else if  (isAMatch(code, THREE) || isAMatch(code, OTHERTHREE)) result = 3.toString
    else if  (isAMatch(code, FIVE) || isAMatch(code, OTHERFIVE)) result = 5.toString
    else if  (isAMatch(code, SIX) || isAMatch(code, OTHERSIX)) result = 6.toString
    else if  (isAMatch(code, NINE) || isAMatch(code, OTHERNINE)) result = 9.toString
    else result = ""
    //println("CANNOT DECIPHER 6 digs " + code)
    result
   }
//2,3,5
  def decipherValue5(code: String): String = {
    var result:String = ""
   if (isAMatch(code, TWO) || isAMatch(code, OTHERTWO)) result = 2.toString
    else if  (isAMatch(code, THREE) || isAMatch(code, OTHERTHREE)  || isAMatch(code, OTHERTHREE2)|| isAMatch(code, OTHERTHREE3)|| isAMatch(code, OTHERTHREE4)|| isAMatch(code, OTHERTHREE5)) result = 3.toString
    else if  (isAMatch(code, FIVE) || isAMatch(code, OTHERFIVE) || isAMatch(code, OTHERFIVE2)|| isAMatch(code, OTHERFIVE3)|| isAMatch(code, OTHERFIVE4)) result = 5.toString
    else {
     result = ""
     println("CANNOT DECIPHER 5 digs " + code)
   }
    result
  }

  //9,6,0
  def decipherValue6(code: String): String = {
    var result:String = ""
    if (isAMatch(code, ZERO) || isAMatch(code, OTHERZERO)|| isAMatch(code, OTHERZERO1)) result = 0.toString
    else if  (isAMatch(code, SIX) || isAMatch(code, OTHERSIX)) result = 6.toString
    else if  (isAMatch(code, NINE) || isAMatch(code, OTHERNINE) || isAMatch(code, OTHERNINE2)|| isAMatch(code, OTHERNINE3)|| isAMatch(code, OTHERNINE4)|| isAMatch(code, OTHERNINE5)) result = 9.toString
    else {
      result = ""
      println("CANNOT DECIPHER 6 digs " + code)
    }
    result
  }

  def decipher(input: Int, code: String): String = {

    input match {
     case 5 => return decipherValue5(code)
      case 6 => return decipherValue6(code)
      case 2 => return 1.toString
      case 4 => return 4.toString
      case 3 => return 7.toString
      case 7 => return 8.toString

    }

  }

  @tailrec
  def decipherAndSumOutput(index: Int): Boolean = {

    if (index >= inputList.length) return true
    else {
      var outputValue: String = ""
      val outputs = inputList.apply(index).split('|')
      outputs.apply(1).trim.split(' ').foreach(code => outputValue = outputValue + decipher(code.trim.length, code.trim) )
     if(outputValue.length > 0) OUTPUTSUM = OUTPUTSUM + (outputValue.trim).toLong
    }
    decipherAndSumOutput(index + 1)
  }


  initializeKnownDigitCountMap()
  decipherAndSumOutput(0)
  println("OUTPUTSUM: " + OUTPUTSUM)

  //store count of output 1,4,7,8 in a map
  //sum the four digits


}

