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
 
func hash(_ str: String) -> Int {
    var sum = 0

    for c in str {
        sum += Int(c.asciiValue!)
        sum *= 17
        sum = sum % 256
    }

    return sum
}

let line = readFile("input.txt")[0]

var sum = 0

let segments = line.split(separator: ",")

for segment in segments {
    sum += hash(String(segment))
}

print(sum)

struct Lens {
    var label: String
    var focal: Int
}

func insert(_ boxes: inout [[Lens]], _ seg: String) {
    let idx = seg.firstIndex(of: "=")!
    let label = seg[seg.startIndex..<idx]
    let focalidx = seg.index(after: idx)
    let focal = Int(seg[focalidx..<seg.endIndex])!

    let box = hash(String(label))

    for (i, dest) in boxes[box].enumerated() {
        if dest.label == label {
            boxes[box][i].focal = focal
            return
        }
    }

    boxes[box].append(Lens(label: String(label), focal: focal))
}

func remove(_ boxes: inout [[Lens]], _ seg: String) {
    let idx = seg.firstIndex(of: "-")!
    let label = seg[seg.startIndex..<idx]

    let box = hash(String(label))

    for (i, dest) in boxes[box].enumerated() {
        if dest.label == label {
            boxes[box].remove(at: i)
            return
        }
    }
}

var boxes: Array<Array<Lens>> = Array(repeating: [], count: 256)

for segment in segments {
    let seg = String(segment)

    if let i = seg.firstIndex(of: "=") {
        insert(&boxes, seg)
    }

    if let i = seg.firstIndex(of: "-") {
        remove(&boxes, seg)
    }
}

var totalpower = 0

for (idx, box) in boxes.enumerated() {
    for (slot, lens) in box.enumerated() {
        totalpower += (1 + idx) * (1 + slot) * lens.focal
    }
}

print(totalpower)
