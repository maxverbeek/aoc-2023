import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer
object Main {

  def clamp(idx: (Int, Int), height: Int, width: Int): (Int, Int) = {
    val i = Math.max(Math.min(idx._1, height - 1), 0)
    val j = Math.max(Math.min(idx._2, width - 1), 0)

    (i, j)
  }

  def surrounds(lines: Array[String], num: String, pos: (Int, Int)): Boolean = {
    val top = pos._1
    val left = pos._2

    for (y <- (top - 1) to (top + 1)) {
      for (x <- (left - 1) to (left + num.length)) {
        val (i, j) = clamp((y, x), lines.length, lines.head.length)

        if (!lines(i)(j).isDigit && lines(i)(j) != '.') {
          return true
        }
      }
    }

    false
  }

  def findPredicate(lines: Array[String], p: Char => Boolean): Array[(String, (Int, Int))] = {
    var numbers: ArrayBuffer[(String, (Int, Int))] = ArrayBuffer()

    for ((line, i) <- lines.zipWithIndex) {
      var currentNumber = "";
      for ((c, j) <- line.zipWithIndex) {
        if (p(c)) {
          currentNumber = currentNumber.appended(c)
        } else if (currentNumber != "") {
          // just ended a number
          val pos = (i, j - currentNumber.length)
          numbers.append((currentNumber, pos))
          currentNumber = ""
        }
      }

      if (currentNumber != "") {
          // just ended a number because the string ended
          val pos = (i, line.length - currentNumber.length)
          numbers.append((currentNumber, pos))
          currentNumber = ""
      }
    }

    return numbers.toArray
  }

  def findGears(lines: Array[String], asterisks: Array[(Int, Int)], locnum: Map[(Int, Int), Int]): Int = {
    var sum = 0
    for ((top, left) <- asterisks.iterator) {
      var locs = ArrayBuffer[(Int, Int)]()

      for (y <- (top - 1) to (top + 1)) {
        for (x <- (left - 1) to (left + 1)) {
          val (i, j) = clamp((y, x), lines.length, lines.head.length)

          if (lines(i)(j).isDigit) {
            // traverse to the left as much as possible
            var start = j
            while (start > 0 && lines(i)(start - 1).isDigit) {
              start -= 1
            }

            locs.append((i, start))
          }
        }
      }
      val uniquelocs = locs.distinct

      if (uniquelocs.length == 2) {

        sum = sum + uniquelocs.map(locnum).product
      }
    }

    return sum
  }


  def main(args: Array[String]): Unit = {
    val lines = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != null).toArray

    val numbers = findPredicate(lines, _.isDigit)
    val surrounded = numbers.filter { case (num, pos) => surrounds(lines, num, pos)}
    val sum = surrounded.map { case (num, _) => num }.map(_.toInt).sum

    println(s"part 1: $sum")

    // find asterisks
    val asterisks = findPredicate(lines, _ == '*').map { case (_, pos ) => pos }
    val numberMapping = numbers.map { case (num, pos) => (pos, num.toInt) }.toMap
    val gearsum = findGears(lines, asterisks, numberMapping)

    println(s"part 2: $gearsum")
  }
}
