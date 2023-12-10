import Foundation
 
//read text file line by line
func readFile(_ path: String) -> [String] {
    errno = 0
    if freopen(path, "r", stdin) == nil {
        print("error")
        return []
    }
    var lines: [String] = []
    while let line = readLine() {
        lines.append(line)
    }
    return lines
}

typealias Direction = (dy: Int, dx: Int)
typealias Location = (y: Int, x: Int)

func ==(lhs: Location, rhs: Location) -> Bool {
    return lhs.y == rhs.y && lhs.x == rhs.x
}

func ~=(lhs: Direction, rhs: Direction) -> Bool {
    return lhs.dy == rhs.dy && lhs.dx == rhs.dx
}

func +(lhs: Location, rhs: Direction) -> Location {
    return (y: lhs.y + rhs.dy, x: lhs.x + rhs.dx)
}

let north: Direction = (dy: -1, dx: 0)
let east: Direction = (dy: 0, dx: 1)
let south: Direction = (dy: 1, dx: 0)
let west: Direction = (dy: 0, dx: -1)

func letterToDir(_ letter: Character) -> Direction? {
    switch letter {
    case "N": return north
    case "E": return east
    case "S": return south
    case "W": return west
    case _: print("letter does not have direction: ", letter); assert(false)
    }
}

func dirToLetter(_ dir: Direction) -> Character {
    switch dir {
    case north: return "N"
    case east: return "E"
    case south: return "S"
    case west: return "W"
    case _: print("direction does not have letter", dir); assert(false)
    }
}

func findStart(_ grid: [String]) -> Location? {
    for (rowindex, row) in grid.enumerated() {
        if let col = row.firstIndex(of: "S") {
            return (y: rowindex, x: row.distance(from: row.startIndex, to: col))
        }
    }

    return nil
}

func lookup(map: [String], y: Int, x: Int) -> Character? {
    guard y >= 0 && y < map.count && x >= 0 && x < map[y].count else {
        return nil
    }

    let row = map[y]
    let index = row.index(row.startIndex, offsetBy: x)
    return row[index]
}

func lookup(map: [String], pos: Location) -> Character? {
    return lookup(map: map, y: pos.y, x: pos.x)
}

func visit(map: inout [String], location: Location, marker: Character) {
    let (y, x) = location
    var row = Array(map[y])
    row[x] = marker
    map[y] = String(row)
}

func move(map: [String], current: Location, facing: Direction) -> Direction? {
    let char = lookup(map: map, y: current.y, x: current.x)
    switch (char, facing) {
    case (.some("|"), north): return north
    case (.some("|"), south): return south
    case (.some("-"), east): return east
    case (.some("-"), west): return west
    case (.some("L"), south): return east
    case (.some("L"), west): return north
    case (.some("J"), south): return west
    case (.some("J"), east): return north
    case (.some("7"), east): return south
    case (.some("7"), north): return west
    case (.some("F"), north): return east
    case (.some("F"), west): return south
    case (.some("S"), _): return facing
    case (_, _): return nil
    }
}

func walk(map: inout [String], start: Location, facing initialdir: Direction) -> Int? {
    var position = start
    var distance = 0
    var facing = initialdir

    var marker: Character = "X"

    while let newdir = move(map: map, current: position, facing: facing) {
        let marker = dirToLetter(newdir)

        let oldpos = position
        visit(map: &map, location: oldpos, marker: marker)
        position = position + newdir
        distance += 1
        facing = newdir

        if position == start {
            // completed the circle
            visit(map: &map, location: position, marker: dirToLetter(initialdir))
            print("completed the circle")
            return distance
        }
    }

    return nil
}

func rotateLeft(_ dir: Direction) -> Direction {
    switch dir {
    case north: return west
    case east: return north
    case south: return east
    case west: return south
    case _: assert(false)
    }
}

func rotateRight(_ dir: Direction) -> Direction {
    switch dir {
    case north: return east
    case east: return south
    case south: return west
    case west: return north
    case _: assert(false)
    }
}

func walk2(map: inout [String], start: Location) {
    // first replace all non-path characters with a .
    let dirchars: Set<Character> = ["N", "E", "S", "W"]
    for (y, line) in map.enumerated() {
        map[y] = String(line.map({ dirchars.contains($0) ? $0 : "." }))
    }

    // start at the start position and keep walking until we are at the start again
    var pos = start
    repeat {
        let dir = letterToDir(lookup(map: map, pos: pos)!)!

        // segment everything in "to the left" and "to the right" of the path
        // test if the neighbouring characters are part of the path
        let left = pos + rotateLeft(dir)
        if let char = lookup(map: map, pos: left), char == "." {
            visit(map: &map, location: left, marker: "L")
        }

        let right = pos + rotateRight(dir)
        if let char = lookup(map: map, pos: right), char == "." {
            visit(map: &map, location: right, marker: "R")
        }
        
        let oldpos = pos
        pos = pos + dir
    } while pos != start
}

func floodFillAt(map: inout [String], location: Location, target: Character) {
    visit(map: &map, location: location, marker: target)

    for d in [north, east, south, west] {
        let neighbour = location + d
        if let c = lookup(map: map, pos: neighbour), c == "." {
            visit(map: &map, location: neighbour, marker: target)
            floodFillAt(map: &map, location: neighbour, target: target)
        }
    }
}

func floodFill(map: inout [String], target: Character) {
    for (y, line) in map.enumerated() {
        for (x, char) in line.enumerated() {

            if char == target {
                let loc: Location = (y: y, x: x)
                floodFillAt(map: &map, location: loc, target: target)
            }
        }
    }
}

func countChars(map: [String], target: Character) -> Int {
    var count = 0
    for line in map {
        for c in line {
            if c == target {
                count += 1
            }
        }
    }

    return count
}

let path = "input.txt"
let map = readFile(path)
let start = findStart(map)!
let char = lookup(map: map, y: start.y, x: start.x)

for d in [north, east, south, west] {
    var mutmap = map
    if let distance = walk(map: &mutmap, start: start, facing: d) {
        print("part 1: ", distance / 2)
        print(String(mutmap.joined(separator: "\n")))
        print("path")
        walk2(map: &mutmap, start: start)
        print(String(mutmap.joined(separator: "\n")))
        print("marked")
        floodFill(map: &mutmap, target: "L")
        floodFill(map: &mutmap, target: "R")
        print(String(mutmap.joined(separator: "\n")))
        print("filled")
        print(countChars(map: mutmap, target: "L"), countChars(map: mutmap, target: "R"))
        break
    }
}
