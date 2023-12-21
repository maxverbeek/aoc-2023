import kotlin.collections.HashMap

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

        if (i == steps) {
        val count = visited.flatten().count { p -> p }
            println("part 1: $count")
        }
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

    // it is useful to keep track of whether the nr of steps is even or odd, because like a chessboard where you can
    // only move in a +, you can only reach black squares from white squares, and white squares from black squares.
    // at some point the entire grid is filled, and will alternate between having the "even" squares filled, and having
    // "odd" squares filled. if the total nr of steps is an even number, we only care about repetition on the even squares.
    val steps = 500
    val evenodd = steps % 2;

    var plotsreachedatstep = Array<Int>(steps + 1) { 0 };

    // keep a map of nr of steps taken in each grid (coordinate divided by size)
    var previoussteps = HashMap<Pair<Int, Int>, Long>()
    var newsteps = HashMap<Pair<Int, Int>, Long>()

    // for grids that are repeating, save the number that they are repeating at.
    var frozengrids = HashMap<Pair<Int, Int>, Long>()

    for (i in 0..steps) {
        var visited = HashSet<Pair<Int, Int>>()
        while (!queue1.isEmpty()) {
            val step = queue1.removeFirst()
            val row = step.first
            val col = step.second

            if (!visited.contains(step) && lines[Math.floorMod(row, height)][Math.floorMod(col, width)] != '#') {
                visited.add(step)

                for (neighbour in neighbours2(step, visited)) {
                    // don't add this neighbour if we can precompute the entire grid that it belongs to
                    val grid = Pair(neighbour.first / height, neighbour.second / width)
                    if (frozengrids.containsKey(grid)) {
                        continue
                    }

                    queue2.addLast(neighbour)
                }
            }
        }

        if (i % 2 == evenodd) {
            // count steps per grid repetition
            for (s in visited) {
                val row = s.first / height
                val col = s.second / width

                newsteps[Pair(row, col)] = newsteps.getOrDefault(Pair(row, col), 0) + 1
            }

            // see if any new grids are repeating, and should be frozen
            for ((grid1, steps1) in previoussteps) {
                if (steps1 == newsteps[grid1]) {
                    println("$grid1 is the same ($steps1) as two steps ago.")
                    frozengrids[grid1] = steps1;
                }
            }

            val tmp1 = previoussteps
            previoussteps = newsteps
            newsteps = tmp1
            newsteps.clear()
        }

        // swap the queues
        val tmp2 = queue2
        queue2 = queue1
        queue1 = tmp2

        plotsreachedatstep[i] = visited.size;
    }

//    plotsreachedatstep.withIndex().forEach { (idx, value) -> println("$idx -> $value") }
//
//    for (i in 2..steps) {
//        val difference = plotsreachedatstep[i] - plotsreachedatstep[i - 2]
//        println("step $i - ${i-2}: $difference")
//    }
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