fun main() {
    var lines = generateSequence { readLine() }
        .takeWhile { it != null }
        .toList()

    try {
        println("part 1: %d".format(solve1(lines)))
    } catch (e: NoSuchElementException) {
        println("skip part1")
    }

    println("part 2: %d".format(solve2(lines)))
}

fun solve1(lines: List<String>): Int {
    val firstDigits = lines.map { it.first({ c -> c.isDigit() }).digitToInt() }
    val lastDigits = lines.map { it.last({ c -> c.isDigit() }).digitToInt() }

    val sum1 = firstDigits.zip(lastDigits).map { (f, l) -> f * 10 + l }.sum()

    return sum1
}

fun solve2(lines: List<String>): Int {

    val superregex = Regex("(?=(\\d|one|two|three|four|five|six|seven|eight|nine))")

    val matches = lines.map {
        val matches = superregex.findAll(it)

        Pair(matches.firstOrNull()!!.groupValues[1], matches.lastOrNull()!!.groupValues[1])
    }

    val firstlast = matches.map { (first, second) -> toNumber(first) * 10 + toNumber(second) }

    return firstlast.sum()
}

val wordnumbers = mapOf(
    "one" to 1,
    "two" to 2,
    "three" to 3,
    "four" to 4,
    "five" to 5,
    "six" to 6,
    "seven" to 7,
    "eight" to 8,
    "nine" to 9
)

fun toNumber(str: String): Int {
    if (str[0].isDigit()) {
        return str[0].digitToInt()
    }

    return wordnumbers[str]!!
}

main()
