use std::{
    cmp::{max, min},
    collections::BTreeMap,
    fmt::Display,
    io::stdin,
    ops::Add,
    unreachable,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn deltas(&self) -> Tuple2 {
        match &self {
            Direction::Up => Tuple2(-1, 0),
            Direction::Right => Tuple2(0, 1),
            Direction::Down => Tuple2(1, 0),
            Direction::Left => Tuple2(0, -1),
        }
    }

    fn to_char(&self) -> char {
        match &self {
            Direction::Up => '^',
            Direction::Right => '>',
            Direction::Down => 'V',
            Direction::Left => '<',
        }
    }
}

impl From<&str> for Direction {
    fn from(value: &str) -> Self {
        match value {
            "U" => Self::Up,
            "R" => Self::Right,
            "D" => Self::Down,
            "L" => Self::Left,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
struct Tuple2(isize, isize);

impl Display for Tuple2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl Add<&Tuple2> for Tuple2 {
    type Output = Tuple2;

    fn add(self, rhs: &Tuple2) -> Self::Output {
        Tuple2(self.0 + rhs.0, self.1 + rhs.1)
    }
}

#[derive(Debug)]
struct ColorStep {
    direction: Direction,
    distance: isize,
}

impl From<&str> for ColorStep {
    fn from(value: &str) -> Self {
        let parts: Vec<&str> = value.split(' ').collect();

        match parts.as_slice() {
            [dir, dist, color] => {
                let diststr = &color[2..7];
                let dirstr = &color[7..8];

                let dir = match dirstr {
                    "0" => Direction::Right,
                    "1" => Direction::Down,
                    "2" => Direction::Left,
                    "3" => Direction::Up,
                    _ => panic!("bad direction number"),
                };

                let dist =
                    isize::from_str_radix(diststr, 16).expect("invalid hexadecimal distance");

                Self {
                    direction: dir,
                    distance: dist,
                }
            }
            _ => panic!("bad input"),
        }
    }
}

#[derive(Debug)]
struct Step {
    direction: Direction,
    distance: isize,
}

impl From<&str> for Step {
    fn from(value: &str) -> Self {
        let parts: Vec<&str> = value.split(' ').collect();

        match parts.as_slice() {
            [dir, dist, color] => Self {
                direction: (*dir).into(),
                distance: (*dist).parse().expect("must have valid input"),
            },
            _ => panic!("bad input"),
        }
    }
}

impl From<ColorStep> for Step {
    fn from(value: ColorStep) -> Self {
        Self {
            distance: value.distance,
            direction: value.direction,
        }
    }
}

fn part1(steps: Vec<Step>) {
    let mut map: BTreeMap<Tuple2, Direction> = BTreeMap::new();
    let mut cur = Tuple2(0, 0);

    let mut minrow = 0;
    let mut maxrow = 0;
    let mut mincol = 0;
    let mut maxcol = 0;

    for step in steps.iter() {
        // do a thing where vertical takes precedence over horizontal
        if step.direction == Direction::Up || step.direction == Direction::Down {
            map.insert(cur, step.direction);
        }

        for _d in 0..step.distance {
            cur = cur + &step.direction.deltas();
            map.insert(cur, step.direction);

            minrow = min(cur.0, minrow);
            mincol = min(cur.1, mincol);

            maxrow = max(cur.0, maxrow);
            maxcol = max(cur.1, maxcol);
        }
    }

    let height: usize = usize::try_from(maxrow - minrow).unwrap() + 1;
    let width: usize = usize::try_from(maxcol - mincol).unwrap() + 1;

    let mut grid: Vec<Vec<Option<Direction>>> = vec![vec![None; width]; height];

    for (Tuple2(row, col), &d) in map.iter() {
        let row: usize = usize::try_from(row - minrow).unwrap();
        let col: usize = usize::try_from(col - mincol).unwrap();

        grid[row][col] = Some(d);
    }

    // print_grid(&grid);

    println!("part 1: {}", scanline(&grid) + map.len());
}

fn part2(steps: Vec<Step>) {
    let mut coords = vec![Tuple2(0, 0)];
    let mut total_distance = 0;

    // idea: subdivide the area into triangles. Each triangle is made up of
    // 3 points: (0, 0); v1; v2.
    // if we add up the surface area of all triangles we end up with the surface area of the total
    // area. Due to the winding of the triangle, if at any point we go around a corner, the area of
    // the triangle becomes negative. so this will still work.
    for step in steps.iter() {
        let last = coords.last().unwrap();

        let Tuple2(dr, dc) = step.direction.deltas();
        let rows = dr * step.distance;
        let cols = dc * step.distance;

        let new = Tuple2(last.0 + rows, last.1 + cols);
        coords.push(new);
        total_distance += step.distance;
    }

    let mut sum = 0;

    for i in 0..(coords.len() - 1) {
        let j = i + 1;
        let Tuple2(row1, col1) = coords[i];
        let Tuple2(row2, col2) = coords[j];

        // area of a triangle is the below formula divided by 2. I'm dividing at the end to avoid
        // rounding errors.
        sum += row2 * col1 - row1 * col2;
    }

    sum /= 2;

    // after having summed the triangle areas we need to consider how the line moves around
    // squares. my assumption is that each vertex is a coordinate that lies in the exact center of
    // a square. When doing so we added too little surface area: the surface area that is missing
    // is half of the surface area of the perimeter. All my answers are off by 1 so I added that
    // too. I'm not sure where this 1 extra surface went.
    println!("part 2: {}", sum + total_distance / 2 + 1);
}

fn main() {
    let lines: Vec<String> = stdin().lines().map(|l| l.unwrap()).collect();
    let part1steps: Vec<Step> = lines
        .iter()
        .cloned()
        .map(|l| Into::<Step>::into(l.as_str()))
        .collect();

    let part2steps: Vec<Step> = lines
        .iter()
        .map(|l| Into::<Step>::into(Into::<ColorStep>::into(l.as_str())))
        .collect();

    part1(part1steps);
    part2(part2steps);
}

fn print_grid(grid: &Vec<Vec<Option<Direction>>>) {
    for line in grid.iter() {
        println!(
            "{}",
            line.iter()
                .map(|d| d.map_or('.', |d| d.to_char()))
                .collect::<String>()
        );
    }
}

fn scanline(grid: &Vec<Vec<Option<Direction>>>) -> usize {
    let mut count = 0;

    let width = grid[0].len();
    let height = grid.len();

    let opening: Direction = grid
        .iter()
        .flatten()
        .find(|d| **d == Some(Direction::Up) || **d == Some(Direction::Down))
        .unwrap()
        .unwrap();

    for i in 0..height {
        let mut inside = false;
        for j in 0..width {
            let c = grid[i][j];

            match c {
                Some(Direction::Up) | Some(Direction::Down) => inside = c == Some(opening),
                Some(_) => {}
                None => {
                    if inside {
                        count += 1;
                    }
                }
            }
        }
    }

    count
}
