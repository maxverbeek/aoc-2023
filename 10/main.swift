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
        if newdir.dy < 0 {
            marker = "N"
        } else if newdir.dy > 0 {
            marker = "S"
        } else if newdir.dx < 0 {
            marker = "W"
        } else {
            marker = "E"
        }

        let oldpos = position
        visit(map: &map, location: oldpos, marker: marker)
        position = position + newdir
        distance += 1
        facing = newdir

        if position == start {
            // completed the circle
            visit(map: &map, location: position, marker: marker)
            print("completed the circle")
            return distance
        }
    }

    return nil
}

func countInlcuded(map: [String]) -> (Int, Int) {
    // first determine what the inside and the outside of the path is.
    // when approaching from the left of the grid it is impossible to switch north/south directions

    let directioncharacters: Set<Character> = ["N", "S", "E", "W", "\n"]

    var northsouth: Character = "X"

    for line in map {
        let dir = line.first(where: { directioncharacters.contains($0) })!
        if dir != "N" && dir != "S" {
            continue
        }

        northsouth = dir
        break
    }

    var inside = 0
    var outside = 0
    
    assert(northsouth == "N" || northsouth == "S")

    let inmarker: Character = northsouth == "N" ? "N" : "S"
    let outmarker: Character = northsouth == "N" ? "S" : "N"

    for line in map {
        var isIn = false
        
        for char in line {
            if char == inmarker {
                isIn = true
            } else if char == outmarker {
                isIn = false
            }

            if !directioncharacters.contains(char) {
                if isIn {
                    inside += 1
                } else {
                    outside += 1
                }
            }
        }
    }

    return (inside, outside)
}

let path = "example.txt"
let map = readFile(path)
let start = findStart(map)!
let char = lookup(map: map, y: start.y, x: start.x)

for d in [north, east, south, west] {
    var mutmap = map
    if let distance = walk(map: &mutmap, start: start, facing: d) {
        print("part 1: ", distance / 2)
        print("part 2: ", countInlcuded(map: mutmap))
        let directioncharacters: Set<Character> = ["N", "S", "E", "W", "\n"]
        print(String(mutmap.joined(separator: "\n").map({ directioncharacters.contains($0) ? $0 : "."})))
        break
    }
}
