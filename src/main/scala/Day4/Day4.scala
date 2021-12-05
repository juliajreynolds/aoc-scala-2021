package Day4

import aoc.AoCUtils.{parse_input_for_iterator, parse_string_input}

import scala.annotation.tailrec
import scala.collection.mutable

object Day4 extends App {

  val inputIterator = parse_input_for_iterator("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day4/input.txt")

  //get balls from input line 1
  val balls = inputIterator.next().split(',').map(_.toInt)
  //get cards from input line 2 ->
  val cards = inputIterator.sliding(5,5).map(_.toArray).
    map(_.map(row => row.trim.split("\\D+").map(value => (value.toInt, 0)))).
    toArray

  @tailrec def calculateScore(balls: Array[Int], cards: Array[Array[Array[(Int, Int)]]], position: Int = 0): Int = {
    val result = cards.
      filter(block => !block.filter(row => row.foldLeft(0)(_ + _._2) == 5).isEmpty ||
        !block.transpose.filter(row => row.foldLeft(0)(_ + _._2) == 5).isEmpty)

    if(result.isEmpty){
      val moddata = cards.map(block => {
        block.map(row => {
          row.map(value => {
            if(value._1 == balls(position)) (value._1, 1) else value
          })
        })
      })
      calculateScore(balls, moddata, position + 1)
    }
    else {
      result.
        head.
        reduce(Array.concat(_, _)).
        foldLeft(0)( (acc, value) => if(value._2 == 0) acc + value._1 else acc ) * balls(position - 1)
    }
  }
  @tailrec def calculateLoser(balls: Array[Int], cards: Array[Array[Array[(Int, Int)]]], position: Int = 0): Int = {
    val uncompleteblocks = cards.
      filter(block => block.filter(row => row.foldLeft(0)(_ + _._2) == 5).isEmpty &&
        block.transpose.filter(row => row.foldLeft(0)(_ + _._2) == 5).isEmpty)

    if(uncompleteblocks.isEmpty && cards.size == 1)
      cards.
        head.
        reduce(Array.concat(_, _)).
        foldLeft(0)( (acc, value) => if(value._2 == 0) acc + value._1 else acc ) * balls(position - 1)
    else {
      val moddata = uncompleteblocks.map(block => {
        block.map(row => {
          row.map(value => {
            if(value._1 == balls(position)) (value._1, 1) else value
          })
        })
      })
      calculateLoser(balls, moddata, position + 1)
    }
  }

//read and match balls until a complete row or column is found
  //sum all unmatched numbers on winning card
  //multiply sum with last ball to get score

  println(calculateScore(balls,cards))
  println(calculateLoser(balls,cards))
}

