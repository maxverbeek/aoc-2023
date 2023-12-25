import sys

def parse_line(line):
    [pos, vel] = line.split(' @ ')
    [x, y, z] = [ int(n) for n in pos.split(', ') ]
    [dx, dy, dz] = [ int (n) for n in vel.split(', ') ]
    return (x, y, z, dx, dy, dz)

lines = [ parse_line(l) for l in sys.stdin ]

def inbounds(x):
    # return 7 <= x and x <= 27
    return 200000000000000 <= x and x < 400000000000000

def find_intersection(l1, l2):
    x1, y1, z1, dx1, dy1, dz1 = l1
    x2, y2, z2, dx2, dy2, dz2 = l2

    # Check if the lines are parallel
    # det = dx1 - dx2 + dy1 - dy2
    det = dx1 * dy2 - dx2 * dy1
    if det == 0:
        # no intersection
        return None

    # Solve for t and s
    t = ((x2 - x1) * dy2 - (y2 - y1) * dx2) / det
    s = ((x2 - x1) * dy1 - (y2 - y1) * dx1) / det

    # Calculate intersection point
    x = x1 + t * dx1
    y = y1 + t * dy1
    z = z1 + t * dz1

    return x, y, z, t, s

times = {}

def addintersection(i, t, j, s):
    iexists = i not in times or times[i] > t
    jexists = j not in times or times[j] > s

    print(f"{iexists=} {jexists=}")

    if iexists and jexists:
        times[i] = t
        times[j] = s
        return

def part1(i, j):
    l1 = lines[i]
    l2 = lines[j]

    intersect = find_intersection(l1, l2)

    if intersect != None:
        x, y, z, t, s = intersect

        if inbounds(x) and inbounds(y) and t > 0 and s > 0:
            print(f"{l1} x {l2} intersect at {x}, {y} (in bounds)")
            return True

        print(f"{l1} x {l2} intersect at {x}, {y} (not in bounds)")
    else:
        print(f"{l1} and {l2} never intersect")

    return False

p1count = 0

for i in range(len(lines)):
    l1 = lines[i]

    for j in range(i+1, len(lines)):
        if part1(i, j):
            p1count += 1

print(p1count)
