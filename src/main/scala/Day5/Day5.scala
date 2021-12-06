package Day5

import Day2.Day2.{depth, horizontal, inputList}
import aoc.AoCUtils.parse_string_input

import scala.annotation.tailrec

object Day5  extends App{

  val inputList = parse_string_input("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day5/input.txt")
  var coordinateMap = scala.collection.mutable.Map[Position,Int]().withDefaultValue(0)

  case class Position(row: Int, col: Int)

  object Position{
    def apply(inputString:String): Position = {
      Position(inputString.split(',').apply(0).toInt,inputString.split(',').apply(1).toInt)
    }
  }

  case class Line(start:Position,end:Position) {

    def isNotDiagonal(): Boolean = {
      if (start.row.equals(end.row) || start.col.equals(end.col)) true
      else false
    }

    def getAllPositions(): IndexedSeq[Position] = {
      var xIncrement: Int = 1
      var yIncrement: Int = 1
      if(start.row > end.row) xIncrement = -1
      if(start.col > end.col) yIncrement = -1
      val rangeX:Range = start.row to end.row by xIncrement
      val rangeY:Range = start.col to end.col by yIncrement
      var index:Int = 0

      val lineLength = if (rangeX.size >= rangeY.size) rangeX.size else rangeY.size
      for(index <- 0 to lineLength-1)
        yield Position(rangeX.apply(if (rangeX.size >= rangeY.size) index else 0), rangeY.apply(if (rangeX.size <= rangeY.size) index else 0))
    }

    def getVerticalPositions(): IndexedSeq[Position] = {
      var xIncrement: Int = 1
      var yIncrement: Int = 1
      if(start.row > end.row) xIncrement = -1
      if(start.col > end.col) yIncrement = -1
      val rangeX:Range = start.row to end.row by xIncrement
      val rangeY:Range = start.col to end.col by yIncrement
      var index:Int = 0

      for(index <- 0 to rangeX.size-1)
        yield Position(rangeX.apply(index), rangeY.apply(index))
    }
  }

  object Line{
    def apply(inputString:String): Line = {
       Line(Position( inputString.split(" -> ").apply(0) ) , Position( inputString.split(" -> ").apply(1) ) )
    }
  }

  def recordPositionsInCoordinateMap(line: Line) = {
    val linePositions = line.getAllPositions()
    for( position <- linePositions) {
      coordinateMap(position) += 1
    }
  }

  def recordDiagonalPositionsInCoordinateMap(line: Line) = {
    val linePositions = line.getVerticalPositions()
    for( position <- linePositions) {
      coordinateMap(position) += 1
    }
  }

  //check for vertical or horizontal 2,2 -> 2,1 where x1 = x2 or y1 = y2
  //put the h&v coords into a map with a counter
  @tailrec
  def countHorizontalAndVerticalCoordinates(index:Int):Boolean = {

    if (index >= inputList.length) return true
    else {
      val thisLine = Line(inputList.apply(index))
      if(thisLine.isNotDiagonal()){
        recordPositionsInCoordinateMap(thisLine)
      }
    }
    countHorizontalAndVerticalCoordinates(index + 1)
  }


  def countDiagonalCoordinates(index:Int):Boolean = {
    if (index >= inputList.length) return true
    else {
      val thisLine = Line(inputList.apply(index))
      if(!thisLine.isNotDiagonal()){
        recordDiagonalPositionsInCoordinateMap(thisLine)
      }
    }
    countDiagonalCoordinates(index + 1)
  }

//count the coordinates with more than 1 counter size
  countHorizontalAndVerticalCoordinates(0)
  println("1 - Overlaps: " + coordinateMap.count((t) => t._2 > 1))
  countDiagonalCoordinates(0)
  println("2 - Overlaps: " + coordinateMap.count((t) => t._2 > 1))


}

