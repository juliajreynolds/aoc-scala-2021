package aoc

import scala.io.Source

object AoCUtils {
  def parse_input(filePath: String): List[Int] =
    (for (x <- Source.fromFile(filePath).getLines()) yield x.toInt).toList


}
