package Day6

import aoc.AoCUtils.parse_input_string

import scala.collection.mutable
import scala.language.postfixOps

object Day6 extends App {

  val inputList = parse_input_string("/Users/julia.reynolds/Workspace/aoc-scala-2021/src/main/scala/Day6/input.txt")

  val lanternfishSchool = scala.collection.mutable.ListBuffer.empty[LanternFish]
  var fishCensusMap = new mutable.HashMap[Int, Long]()

  lanternfishSchool.addAll(inputList.toString.split(',').map(entry => new LanternFish(entry.toInt)))

  case class LanternFish(var age: Int) {
    def decrementAge(): Unit = this.age = age - 1

    def adulting(): Unit = this.age = 6
  }

  case class FishCounter(var count: Int = 0) {
    def setCount(newCount: Int): Int = {
      this.count = newCount
      this.count
    }
  }

  def makeSomeBabies(newFishToAdd: Int): Boolean = {
    for (a <- 1 to newFishToAdd) {
      lanternfishSchool.addOne(new LanternFish(8))
    }
    true
  }

  def newDay(day: Int): Boolean = {
    var dayCount: Int = 256

    if (day >= dayCount) return true
    else {
      var newFishToAdd: Int = 0
      lanternfishSchool.map(fish => {

        if (fish.age.equals(0)) {
          newFishToAdd = newFishToAdd + 1
          fish.adulting()
        } else {
          fish.decrementAge()
        }
      })
      makeSomeBabies(newFishToAdd)
    }
    println("Day: " + day)
    newDay(day + 1)
  }

  def newDay2(day: Int): Boolean = {
    var dayCount: Int = 256
    var newCensusMap = new mutable.HashMap[Int, Long]()
    if (day >= dayCount) return true
    else {
      newCensusMap = fishCensusMap.clone()

      val parents = newCensusMap(0)
      fishCensusMap.update(7, newCensusMap(8))
      fishCensusMap.update(6, newCensusMap(7) + parents)
      fishCensusMap.update(5, newCensusMap(6))
      fishCensusMap.update(4, newCensusMap(5))
      fishCensusMap.update(3, newCensusMap(4))
      fishCensusMap.update(2, newCensusMap(3))
      fishCensusMap.update(1, newCensusMap(2))
      fishCensusMap.update(0, newCensusMap(1))
      fishCensusMap.update(8, parents)
//26984457539  |  1214653763
   }
    newDay2(day + 1)
  }

  def initializeFishCensusHashMap(): Boolean = {
    val groupedStartingFish = inputList.split(',').groupBy(_.toInt)
    for (age <- 0 to 8) {
      if (groupedStartingFish.contains(age)) {
        fishCensusMap.put(age, inputList.split(',').groupBy(_.toInt).get(age).get.length.toLong)
      } else {
        fishCensusMap.put(age,0L )
      }
    }
    println(" START lantern fish population :" + fishCensusMap.values.sum)
    true
  }

  initializeFishCensusHashMap()
  newDay2(0)
  println(" END lantern fish population :" + fishCensusMap.values.sum)

}

