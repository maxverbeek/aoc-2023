case class Mapping(destination: Long, source: Long, length: Long) {
  def contains(num: Long): Boolean = source <= num && num < source + length

  def translate(num: Long): Long = {
    num - source + destination
  }

  def produces(num: Long): Boolean = destination <= num && num < destination + length

  def translate_back(num: Long): Long = {
    num - destination + source
  }
}

case class MapBlock(mappings: List[Mapping]) {
  def convert(num: Long): Long = {
    mappings.find(_.contains(num)) match {
      case Some(mapping) => mapping.translate(num)
      case None => num
    }
  }

  def revert(num: Long): Long = {
    mappings.find(_.produces(num)) match {
      case Some(mapping) => mapping.translate_back(num)
      case None => num
    }
  }
}

case class SeedPair(start: Long, length: Long) {
  def contains(num: Long): Boolean = start <= num && num < start + length
}

object Main {

  def applyMaps(mappers: List[MapBlock], num: Long): Long = {
    mappers.foldLeft(num) { (acc, f) => f.convert(acc) }
  }

  def revertMaps(mappers: List[MapBlock], num: Long): Long = {
    mappers.reverse.foldLeft(num) { (acc, f) => f.revert(acc) }
  }

  def main(args: Array[String]) = {
    val lines = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != null).toArray
    val Array(seedLine, blankline, blockLines @ _*) = lines
    val seeds = seedLine.split(":")(1).trim.split(" ").map(_.toLong).toList

    val blocks = lines.foldLeft(List(List.empty[String])) { (acc, line) =>
      if (line.nonEmpty) {
        if (acc.head.isEmpty) acc.tail :+ List(line)
        else acc.init :+ (acc.last :+ line)
      } else acc :+ List.empty[String]
    }.filter(_.nonEmpty).map(_.tail)

    val mappers = blocks.map { block =>
      val mappings = block.map { line =>
        val Array(destination, source, length) = line.split(" ").map(_.toLong)

        Mapping(destination, source, length)
      }

      MapBlock(mappings.toList)
    }

    val smallest = seeds.map { seed =>
      mappers.foldLeft(seed) { (acc, f) => f.convert(acc) }
    }.min

    println(s"part 1: $smallest")

    val seedpairs = seeds.grouped(2).map { case Seq(start, length) => SeedPair(start, length); case _ => throw new RuntimeException("unmatched"); }.toList

    val smallest2 = Iterator.from(0).find { value =>
      val seed = revertMaps(mappers, value)
      seedpairs.exists(_.contains(seed))
    }

    println(s"part 2: $smallest2")
  }
}
