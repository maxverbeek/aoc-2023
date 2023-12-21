fun main(args: Array<String>) {
    var lines = generateSequence { readLine() }
        .takeWhile { it != null }
        .toList().toTypedArray()

    bfs(lines)
    bfs2(lines)
}

fun findStart(lines: Array<String>): Pair<Int, Int> {
    for (i in lines.indices) {
        for (j in lines[i].indices) {
            if (lines[i][j] == 'S') {
                return Pair(i, j);
            }
        }
    }

    return TODO("Provide the return value")
}

fun neighbours(position: Pair<Int, Int>, visited: Array<Array<Boolean>>): List<Pair<Int, Int>> {
    val row = position.first
    val col = position.second

    return listOf(
        Pair(row + 1, col),
        Pair(row, col + 1),
        Pair(row - 1, col),
        Pair(row, col - 1)
    )
        .filter { pair -> pair.first >= 0 && pair.first < visited.size && pair.second >= 0 && pair.second < visited[0].size }
        .filter { pair -> !visited[pair.first][pair.second] }
}

fun bfs(lines: Array<String>) {
    val start = findStart(lines);
    var queue1 = ArrayDeque<Pair<Int, Int>>();
    var queue2 = ArrayDeque<Pair<Int, Int>>();

    queue1.addFirst(start);

    val steps = 64
    for (i in 0..steps) {
        var visited = Array(lines.size) { Array(lines[0].length) { false } }
        while (!queue1.isEmpty()) {
            val step = queue1.removeFirst()
            val row = step.first
            val col = step.second

            if (!visited[row][col] && lines[row][col] != '#') {
                visited[row][col] = true

                for (neighbour in neighbours(step, visited)) {
                    queue2.addLast(neighbour)
                }
            }
        }

        // swap the queues
        val tmp = queue2
        queue2 = queue1
        queue1 = tmp

//        if (i == steps) {
        val count = visited.flatten().count { p -> p }
        println("visited: $count")
//        }
    }
}

fun neighbours2(position: Pair<Int, Int>, visited: Set<Pair<Int, Int>>): List<Pair<Int, Int>> {
    val row = position.first
    val col = position.second

    return listOf(
        Pair(row + 1, col),
        Pair(row, col + 1),
        Pair(row - 1, col),
        Pair(row, col - 1)
    )
        .filter { pair -> !visited.contains(pair) }
}

fun bfs2(lines: Array<String>) {
    val start = findStart(lines);
    var queue1 = ArrayDeque<Pair<Int, Int>>();
    var queue2 = ArrayDeque<Pair<Int, Int>>();

    val height = lines.size
    val width = lines[0].length

    queue1.addFirst(start);

    val steps = 100
    var plotsreachedatstep = Array<Int>(steps + 1) { 0 };

    for (i in 0..steps) {
        var visited = HashSet<Pair<Int, Int>>()
        while (!queue1.isEmpty()) {
            val step = queue1.removeFirst()
            val row = step.first
            val col = step.second

            if (!visited.contains(step) && lines[Math.floorMod(row, height)][Math.floorMod(col, width)] != '#') {
                visited.add(step)

                for (neighbour in neighbours2(step, visited)) {
                    queue2.addLast(neighbour)
                }
            }
        }

        // swap the queues
        val tmp = queue2
        queue2 = queue1
        queue1 = tmp

        plotsreachedatstep[i] = visited.size;

        if (i == steps) {
            val count = visited.size
            println("visited: $count")
        }

        printgrid(lines, visited)
    }

    plotsreachedatstep.withIndex().forEach { (idx, value) -> println("$idx -> $value") }

    for (i in 1..steps) {
        val difference = plotsreachedatstep[i] - plotsreachedatstep[i - 1]
        println("step $i: + $difference")
    }
}

fun printgrid(lines: Array<String>, visited: Set<Pair<Int, Int>>) {
    val height = lines.size
    val width = lines[0].length

    println("Grid: ")

    for (r in -1..1) {
        val offsety = r * height
        for (i in lines.indices) {
            for (c in -1..1) {
                val offsetx = c * width
                for (j in lines[i].indices) {
                    print(if (visited.contains(Pair(offsety + i, offsetx + j))) 'O' else lines[i][j])
                }
            }
            print("\n")
        }
    }


}