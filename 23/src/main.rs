use std::{collections::HashMap, io::stdin};

type VV<T> = Vec<Vec<T>>;
type TileVec = Vec<Vec<char>>;

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
    // dbg!(&distances2);
    // println!("part 2: {}", distances2.iter().max().unwrap());
    let count = tiles
        .iter()
        .map(|r| r.iter())
        .flatten()
        .filter(|c| **c != '#')
        .count();
    println!("chars: {}", count);
}

type Pos = (usize, usize);

// transform the map into a graph, where each node represents a junction. each node will be an
// index in a vector, and contains an array of neighbours, along with a number of steps to get
// there.
fn make_graph(tiles: &TileVec) -> Vec<Vec<(usize, usize)>> {
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

            if get_neighbours(tiles, pos).count() > 2 || pos == start || pos == end {
                // pos is a junction. first we create a new node specifically for this junction so
                // that we can store it in the vec.
                junction_to_node.insert(pos, junction_to_node.len());
            }
        }
    }

    let mut graph = vec![vec![]; junction_to_node.len()];

    for r in 1..(height - 1) {
        for c in 1..(width - 1) {
            let pos = (r, c);

            if junction_to_node.contains_key(&pos) {
                // we are at a junction, walk in every direction until we encounter a neighbouring
                // junction.
            }
        }
    }

    graph
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
