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

from sympy import symbols, Eq, solve

# Define variables
x, y, z = symbols('x y z')
dx, dy, dz = symbols('dx dy dz')
t1, t2, t3, t4, t5, t6 = symbols('t1 t2 t3 t4 t5 t6')

x1, y1, z1, dx1, dy1, dz1 = lines[0]
x2, y2, z2, dx2, dy2, dz2 = lines[1]
x3, y3, z3, dx3, dy3, dz3 = lines[2]
x4, y4, z4, dx4, dy4, dz4 = lines[3]
x5, y5, z5, dx5, dy5, dz5 = lines[4]
x6, y6, z6, dx6, dy6, dz6 = lines[5]

eqs = [
    Eq((x - x1) + (dx - dx1) * t1, 0),
    Eq((y - y1) + (dy - dy1) * t1, 0),
    Eq((z - z1) + (dz - dz1) * t1, 0),

    Eq((x - x2) + (dx - dx2) * t2, 0),
    Eq((y - y2) + (dy - dy2) * t2, 0),
    Eq((z - z2) + (dz - dz2) * t2, 0),

    Eq((x - x3) + (dx - dx3) * t3, 0),
    Eq((y - y3) + (dy - dy3) * t3, 0),
    Eq((z - z3) + (dz - dz3) * t3, 0),

    Eq((x - x4) + (dx - dx4) * t4, 0),
    Eq((y - y4) + (dy - dy4) * t4, 0),
    Eq((z - z4) + (dz - dz4) * t4, 0),

    Eq((x - x5) + (dx - dx5) * t5, 0),
    Eq((y - y5) + (dy - dy5) * t5, 0),
    Eq((z - z5) + (dz - dz5) * t5, 0),

    Eq((x - x6) + (dx - dx6) * t6, 0),
    Eq((y - y6) + (dy - dy6) * t6, 0),
    Eq((z - z6) + (dz - dz6) * t6, 0)
]

solution = solve(eqs, (x, y, z, dx, dy, dz, t1, t2, t3, t4, t5, t6))

print("part 2:")
print(solution[0][0] + solution[0][1] + solution[0][2])
