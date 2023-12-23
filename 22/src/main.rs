use std::{
    cmp::{max, min},
    collections::{HashSet, VecDeque},
    io::stdin,
    println,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Voxel(usize, usize, usize);

impl Voxel {
    fn lower_by(&mut self, amount: usize) {
        self.2 -= amount;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Block(Voxel, Voxel);

impl Block {
    fn lower_by(&mut self, amount: usize) {
        self.0.lower_by(amount);
        self.1.lower_by(amount);
    }

    fn minx(&self) -> usize {
        min(self.0 .0, self.1 .0)
    }

    fn miny(&self) -> usize {
        min(self.0 .1, self.1 .1)
    }

    fn minz(&self) -> usize {
        min(self.0 .2, self.1 .2)
    }

    fn maxx(&self) -> usize {
        max(self.0 .0, self.1 .0)
    }

    fn maxy(&self) -> usize {
        max(self.0 .1, self.1 .1)
    }

    fn maxz(&self) -> usize {
        max(self.0 .2, self.1 .2)
    }
}

// order by height
impl PartialOrd for Block {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.minz().partial_cmp(&other.minz())
    }
}

impl Ord for Block {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.minz().cmp(&other.minz())
    }
}

fn parse_voxel(segment: &str) -> Option<Voxel> {
    let mut it = segment
        .split(',')
        .map(|s| s.parse::<usize>().ok())
        .flatten();

    Some(Voxel(it.next()?, it.next()?, it.next()?))
}

fn parse_block(line: &str) -> Option<Block> {
    let mut it = line.split('~').map(|s| parse_voxel(s)).flatten();

    Some(Block(it.next()?, it.next()?))
}

fn dim_sizes(blocks: &Vec<Block>) -> (usize, usize, usize) {
    let mut maxx = blocks[0].0 .0;
    let mut maxy = blocks[0].0 .1;
    let mut maxz = blocks[0].0 .2;

    // determine the size of the containing array datastructure by finding the lowest and largest
    // coordinates
    for i in 0..blocks.len() {
        let &Block(start, end) = &blocks[i];

        maxx = max(start.0, maxx);
        maxy = max(start.1, maxy);
        maxz = max(start.2, maxz);

        maxx = max(end.0, maxx);
        maxy = max(end.1, maxy);
        maxz = max(end.2, maxz);
    }

    (maxx, maxy, maxz)
}

macro_rules! loopblock {
    ($block:ident, $x:ident, $y:ident, $z:ident, $body:expr) => {
        for $x in $block.minx()..=$block.maxx() {
            for $y in $block.miny()..=$block.maxy() {
                for $z in $block.minz()..=$block.maxz() {
                    $body
                }
            }
        }
    };
}

fn main() {
    let mut blocks: Vec<Block> = stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| parse_block(&s))
        .map(Option::unwrap)
        .collect();

    blocks.sort();

    let mut map = create_map(&blocks);

    let mut blockfell = true;
    while blockfell {
        blockfell = false;
        for (idx, block) in blocks.iter_mut().enumerate() {
            let initz = block.minz();
            let mut lowest = initz;

            'depth: for z in (0..initz).rev() {
                for x in block.minx()..=block.maxx() {
                    for y in block.miny()..=block.maxy() {
                        if map[x][y][z].is_some() {
                            break 'depth;
                        }
                    }
                }
                lowest = z;
            }

            if lowest == initz {
                continue;
            }

            blockfell = true;
            loopblock!(block, x, y, z, {
                map[x][y][z] = None;
            });

            block.lower_by((initz - lowest).try_into().unwrap());

            loopblock!(block, x, y, z, {
                map[x][y][z] = Some(idx);
            });
        }
    }

    // block i is supported by supported_by[i]
    let mut supported_by: Vec<Vec<usize>> = vec![vec![]; blocks.len()];
    let mut supports: Vec<Vec<usize>> = vec![vec![]; blocks.len()];

    for (idx, block) in blocks.iter().enumerate() {
        if block.minz() == 0 {
            continue;
        }

        let belowz = block.minz() - 1;

        let Block(start, end) = block;

        for x in min(start.0, end.0)..=max(start.0, end.0) {
            for y in min(start.1, end.1)..=max(start.1, end.1) {
                if let Some(b) = map[x][y][belowz] {
                    supported_by[idx].push(b);
                    supports[b].push(idx);
                }
            }
        }
    }

    let mut part1 = 0;

    let mut isdroppedby = vec![HashSet::<usize>::new(); blocks.len()];

    // actually solve the problem:
    for (brick, supporting) in supports.iter().enumerate().rev() {
        println!("brick {brick}: {:?}", supporting);

        if supporting.len() > 0
            && !supporting
                .iter()
                .all(|s| supported_by[*s].iter().any(|b| *b != brick))
        {
            // there are some blocks who are only supported by brick. therefore we cannot remove
            // brick
            continue;
        } else {
            part1 += 1;
        }
    }

    println!("part 1: {}", part1);

    let (_, _, maxz) = dim_sizes(&blocks);

    let mut bricksonfloor = vec![0; maxz + 1];
    for b in blocks.iter() {
        bricksonfloor[b.minz()] += 1;
    }

    let mut bricksabovefloor = vec![0; maxz + 1];
    bricksabovefloor[maxz] = 0;
    for z in (0..maxz).rev() {
        bricksabovefloor[z] = bricksabovefloor[z + 1] + bricksonfloor[z + 1];
    }

    print_map(&map);
    dbg!(&bricksabovefloor);

    let mut totaldropped = 0;

    for (brick, supporting) in supports.iter().enumerate().rev() {
        let mut dropped = HashSet::<usize>::new();
        let mut dropping = VecDeque::from([brick]);

        // it turns out this naive approach is fast enough. my plan was to optimize it further with
        // the above arrays that check if an entire floor is dropped, that it could also instantly
        // know how many bricks should be dropped from the floors above. but this is not
        // implemeneted because this approach is already near instant.
        while let Some(dropnew) = dropping.pop_front() {
            if dropped.contains(&dropnew) {
                continue;
            }

            dropped.insert(dropnew);
            // test if dropnew has unsupported bricks
            for brick in supports[dropnew].iter() {
                if supported_by[*brick].iter().all(|b| dropped.contains(b)) {
                    dropping.push_back(*brick);
                }
            }
        }

        println!("for idx {brick} dropped {}", dropped.len() - 1);
        totaldropped += dropped.len() - 1;
    }

    println!("part 2: {}", totaldropped);
}

fn all_on_level_dropped(
    dropped: &HashSet<usize>,
    map: &Vec<Vec<Vec<Option<usize>>>>,
    level: usize,
) -> bool {
    for x in 0..map.len() {
        for y in 0..map[x].len() {
            if let Some(b) = map[x][y][level] {
                if !dropped.contains(&b) {
                    return false;
                }
            }
        }
    }

    return true;
}

fn dropmany(
    dropbrick: usize,
    dropifdropped: &mut Vec<HashSet<usize>>,
    supports: &Vec<Vec<usize>>,
    supported_by: &Vec<Vec<usize>>,
) {
    // try eliminating this current brick
    // all bricks that are exclusively supported by the set of falling bricks are going to
    // drop.
    // if alreadydropped.contains(&dropbrick) {
    //     return alreadydropped;
    // }

    dropifdropped[dropbrick].insert(dropbrick);
    for b in &supports[dropbrick] {
        if supported_by[*b]
            .iter()
            .all(|s| dropifdropped[dropbrick].contains(s))
        {
            // also dropping all of these, since they have no more support
            dropifdropped[dropbrick].insert(*b);
        }
    }

    for b in &supports[dropbrick] {
        if supported_by[*b]
            .iter()
            .all(|s| dropifdropped[dropbrick].contains(s))
        {
            let newdropped: Vec<usize> = dropifdropped[*b].iter().cloned().collect();
            dropifdropped[dropbrick].extend(newdropped);
        }
    }
}

fn create_map(blocks: &Vec<Block>) -> Vec<Vec<Vec<Option<usize>>>> {
    let (maxx, maxy, maxz) = dim_sizes(&blocks);
    let sizex = maxx + 1;
    let sizey = maxy + 1;
    let sizez = maxz + 1;

    let mut map: Vec<Vec<Vec<Option<usize>>>> = vec![vec![vec![None; sizez]; sizey]; sizex];

    for (idx, block) in blocks.iter().enumerate() {
        loopblock!(block, x, y, z, {
            map[x][y][z] = Some(idx);
        })
    }

    map
}

fn print_map(map: &Vec<Vec<Vec<Option<usize>>>>) {
    for z in 0..map[0][0].len() {
        println!("level {z}:");
        for y in 0..map[0].len() {
            for x in 0..map.len() {
                print!(
                    "{}",
                    map[x][y][z].map_or("-".to_owned(), |d| format!("{d}"))
                );
            }

            println!("");
        }
    }
}
