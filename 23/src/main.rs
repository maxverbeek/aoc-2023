use std::{collections::HashMap, io::stdin, println};

type VV<T> = Vec<Vec<T>>;
type TileVec = Vec<Vec<char>>;
type Pos = (usize, usize);

// my part 2 graph has 36 nodes, therefore storing the visited state in a u64 bitstring is possible
#[derive(Clone, Copy)]
struct Bitset(u64);

impl Bitset {
    fn visit(&mut self, node: usize) {
        self.0 = self.0 | 1 << node;
    }

    fn unvisit(&mut self, node: usize) {
        self.0 = self.0 & !(1 << node);
    }

    fn visited(&self, node: usize) -> bool {
        self.0 & (1 << node) > 0
    }

    fn new() -> Self {
        Self(0)
    }
}

fn main() {
    let mut tiles: TileVec = stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| s.chars().collect())
        .collect();

    let width = tiles[0].len();
    // add some rows to the top and bottom so we dont need oob checks
    tiles.insert(0, vec!['#'; width]);
    tiles.push(vec!['#'; width]);
    let height = tiles.len();

    let mut visited = vec![vec![false; width]; height];

    let start: (usize, usize) = (1, 1);
    let end = (tiles.len() - 2, tiles.last().unwrap().len() - 2);

    let distances1 = walk(&tiles, &mut visited, start, end, false);
    println!("part 1: {}", distances1.iter().max().unwrap());

    // let distances2 = walk(&tiles, &mut visited, start, end, true);
    // println!("part 2: {}", distances2.iter().max().unwrap());

    let (graph, junction_to_node) = make_graph(&tiles);
    let distances3 = walk_graph(
        &graph,
        Bitset::new(),
        *junction_to_node
            .get(&start)
            .expect("start must be in graph"),
        *junction_to_node.get(&end).expect("end must be in graph"),
    );

    println!("part 2: {}", distances3.iter().max().unwrap());
}

fn walk_graph(
    graph: &Vec<Vec<(usize, usize)>>,
    mut visited: Bitset,
    cur: usize,
    end: usize,
) -> Vec<usize> {
    if cur == end {
        return vec![0];
    }

    visited.visit(cur);

    let mut distances: Vec<usize> = vec![];

    for (d, next) in &graph[cur] {
        if visited.visited(*next) {
            continue;
        }
        // add d to all distances, because we walked that far
        let newpaths = walk_graph(graph, visited, *next, end)
            .into_iter()
            .map(|dist| dist + d);

        distances.extend(newpaths);
    }

    visited.unvisit(cur);

    distances
}

// transform the map into a graph, where each node represents a junction. each node will be an
// index in a vector, and contains an array of neighbours, along with a number of steps to get
// there.
fn make_graph(tiles: &TileVec) -> (Vec<Vec<(usize, usize)>>, HashMap<Pos, usize>) {
    let mut junction_to_node: HashMap<Pos, usize> = HashMap::new();

    let start: (usize, usize) = (1, 1);
    let end = (tiles.len() - 2, tiles.last().unwrap().len() - 2);

    let height = tiles.len();
    let width = tiles[0].len();

    // use 1 .. boundary - 1 because the boundary is surrounded with wall and not worth checking.
    // this way we dont need to check for out of bounds conditions
    for r in 1..(height - 1) {
        for c in 1..(width - 1) {
            let pos = (r, c);

            if tiles[r][c] == '#' {
                continue;
            }

            if get_neighbours(tiles, pos).count() > 2 || pos == start || pos == end {
                // pos is a junction. first we create a new node specifically for this junction so
                // that we can store it in the vec.
                junction_to_node.insert(pos, junction_to_node.len());
            }
        }
    }

    let mut graph = vec![vec![]; junction_to_node.len()];

    for (&pos, &mynode) in junction_to_node.iter() {
        for n in get_neighbours(&tiles, pos) {
            if let Some((distance, pos2)) = walk_junction(&tiles, n, pos) {
                let node = junction_to_node
                    .get(&pos2)
                    .expect(format!("junction {:?} must have been inserted", pos2).as_ref());

                graph[mynode].push((distance + 1, *node));
            }
        }
    }

    (graph, junction_to_node)
}

fn walk_junction(tiles: &TileVec, cur: Pos, prev: Pos) -> Option<(usize, Pos)> {
    let neighbours: Vec<Pos> = get_neighbours(&tiles, cur).filter(|&n| n != prev).collect();

    if neighbours.len() > 1 {
        // found a new junction at current pos
        return Some((0, cur));
    }

    if let Some(&next) = neighbours.first() {
        // visit the next neighbour
        let (d, junction) = walk_junction(&tiles, next, cur)?;
        return Some((d + 1, junction));
    }

    let start: (usize, usize) = (1, 1);
    let end = (tiles.len() - 2, tiles.last().unwrap().len() - 2);

    // no more neighbours, but start and end also count as a junction
    if cur == start || cur == end {
        return Some((0, cur));
    }

    // found a dead end
    None
}

fn get_neighbours<'a>(tiles: &'a TileVec, pos: Pos) -> impl Iterator<Item = Pos> + 'a {
    let (r, c) = pos;

    [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]
        .into_iter()
        .filter(|&(r, c)| tiles[r][c] != '#')
}

fn walk(
    tiles: &TileVec,
    visited: &mut VV<bool>,
    pos: (usize, usize),
    end: (usize, usize),
    uphill: bool,
) -> Vec<usize> {
    if pos == end {
        return vec![0];
    }

    let (r, c) = pos;
    visited[r][c] = true;

    let neighbours: Vec<(usize, usize)> = [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]
        .into_iter()
        .zip(['v', '>', '^', '<'])
        .filter(|((r, c), d)| valid_neighbour(tiles[*r][*c], *d, uphill))
        .map(|(f, _)| f)
        .filter(|(nr, nc)| !visited[*nr][*nc])
        .collect();

    let mut dist = vec![];

    for (nr, nc) in neighbours {
        let newdist = walk(tiles, visited, (nr, nc), end, uphill);
        // add 1 to all distances because we stepped 1 spot
        dist.append(&mut newdist.into_iter().map(|d| d + 1).collect());
    }

    // allow backtracking so we can revisit this in an alternate universe
    visited[r][c] = false;

    dist
}

fn valid_neighbour(tile: char, dir: char, uphill: bool) -> bool {
    if uphill {
        tile != '#'
    } else {
        tile == '.' || tile == dir
    }
}
