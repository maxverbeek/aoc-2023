import kotlin.collections.HashMap

fun main(args: Array<String>) {
    var lines = generateSequence { readLine() }
        .takeWhile { it != null }
        .toList().toTypedArray()

    bfs(lines)
    bfs2(lines, 26501365);
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

fun bfs2(lines: Array<String>, target: Int) {
    val start = findStart(lines);
    var queue1 = ArrayDeque<Pair<Int, Int>>();
    var queue2 = ArrayDeque<Pair<Int, Int>>();

    val height = lines.size
    val width = lines[0].length

    queue1.addFirst(start);

    // since the target is very very large we cannot iterate to it. However: I noticed that the nr of reachable tiles increases
    // roughly quadratically in the nr of steps.. The idea: I assume that there is some sort periodicity in the width of the grid (131).
    // using this, I want to create a formula f(m + kt) = something in terms of t where ultimately I calculate f(target), and express target in
    // m + kt where k is my period (131). therefore: m shall be the remainder (target % 131) = 65. this means that by substition we end up with
    // f(x) = ... where x = m + kt. we can extract values for f() at k = 0, 1, 2, ... and eventually calculate f() for k = (target - m) / width
    val m = target % width;
    val steps = 4 * width + m;

    var f = Array<Int>(5) { 0 }

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
        val tmp2 = queue2
        queue2 = queue1
        queue1 = tmp2


        if (i % width == 65) {
            println("(65) -- step: $i, visited: ${visited.size}")
            f[i / width] = visited.size;
        }
    }

    f.withIndex().forEach { (i, f) -> println("$i: $f")}
    //    0: 3799
    //    1: 34047
    //    2: 94475
    //    3: 185083
    //    4: 305871

    // Keep evaluating derivatives to determine the constant for the quadratic term (see day 9)
    val df = derivative(f)
    df.withIndex().forEach { (i, f) -> println("$i: $f")}
    //    0: 30248
    //    1: 60428
    //    2: 90608
    //    3: 120788

    val ddf = derivative(df)
    ddf.withIndex().forEach { (i, f) -> println("$i: $f")}
    //    0: 30180
    //    1: 30180
    //    2: 30180

    // now we have f''(x) = 30180
    // -> f'(x) = 30180 * x + C
    // -> f(x) = 15090 * x^2 + Cx + D
    // with -> f(1) = 3799 and f(2) = 34047
    // f(1) = 15090 + C + D     = 3799
    // f(2) = 15090 *4 + 2C + D = 34047
    // -> C  + D = -11291
    // -> 2C + D = -26313
    // -> C = (-26313 - -11291) = -15022
    // -> D = (-11291 - -15022) = 3731
    //
    // -> f(x) = 15090 * x^2 - 15022 * x + 3731

    // since we want to evaluate what the nr of steps is for the target, we need to transform it to a suitable x value:
    // f(m + 0t) = 3799 = f(1)
    // f(m + 1t) = f(2)
    // -> k = (target - m) / t + 1

    val k = (target.toLong() - m.toLong()) / width.toLong() + 1
    val totalsteps: Long = (15090 * k * k) - (15022 * k) + 3731
    println("part 2: $totalsteps")
}

fun derivative(nums: Array<Int>): Array<Int> {
    var res = Array(nums.size - 1) { 0 }

    for (i in 1 until nums.size) {
        res[i - 1] = nums[i] - nums[i - 1]
    }

    return res
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