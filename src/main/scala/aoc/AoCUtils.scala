package aoc

import scala.io.Source

object AoCUtils {
  def parse_input(filePath: String): List[Int] =
    (for (x <- Source.fromFile(filePath).getLines()) yield x.toInt).toList

  def parse_input_string(filePath: String): String =
    Source.fromFile(filePath).getLines().next()

  def parse_input_for_iterator(filePath: String): Iterator[String] =
    Source.fromFile(filePath).getLines().filter(_.nonEmpty)

  def parse_string_input(filePath: String): List[String] =
    (for (x <- Source.fromFile(filePath).getLines()) yield x.toString).toList




}
