fun main() {
    var lines = generateSequence { readLine() }
        .takeWhile { it != null }
        .toList()

    // println("part 1: %d".format(solve(lines)))

    val wordnumbers = mapOf(
        "one" to "1",
        "two" to "2",
        "three" to "3",
        "four" to "4",
        "five" to "5",
        "six" to "6",
        "seven" to "7",
        "eight" to "8",
        "nine" to "9"
    )

    val newlines = lines.map { wordnumbers.entries.fold(it) { acc, (from, to) -> acc.replace(from, to)} }

    print(newlines.joinToString("\n"))

    // println("part 2: %d".format(solve(newlines)))
}

fun solve(lines: List<String>): Int {
    val firstDigits = lines.map { it.first({ c -> c.isDigit() }).digitToInt() }
    val lastDigits = lines.map { it.last({ c -> c.isDigit() }).digitToInt() }

    val sum1 = firstDigits.zip(lastDigits).map { (f, l) -> f * 10 + l }.sum()

    return sum1
}

main()
