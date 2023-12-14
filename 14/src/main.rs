use std::io::BufRead;
use std::collections::{HashMap, hash_map::Entry};

fn find_top(lines: &Vec<Vec<char>>, row: usize, col: usize, target: char) -> Option<usize> {
    let stop = '#';

    let mut result: Option<usize> = None;

    for y in (0..=row).rev() {
        let current = lines[y][col];
        if current == stop {
            break;
        }

        if current == target {
            result = Some(y);
        }
    }

    result
}

fn find_bot(lines: &Vec<Vec<char>>, row: usize, col: usize, target: char) -> Option<usize> {
    let stop = '#';

    let mut result: Option<usize> = None;

    let height = lines.len();

    for y in row..height {
        let current = lines[y][col];
        if current == stop {
            break;
        }

        if current == target {
            result = Some(y);
        }
    }

    result
}
fn find_left(lines: &Vec<Vec<char>>, row: usize, col: usize, target: char) -> Option<usize> {
    let stop = '#';

    let mut result: Option<usize> = None;

    for x in (0..=col).rev() {
        let current = lines[row][x];
        if current == stop {
            break;
        }

        if current == target {
            result = Some(x);
        }
    }

    result
}

fn find_right(lines: &Vec<Vec<char>>, row: usize, col: usize, target: char) -> Option<usize> {
    let stop = '#';

    let mut result: Option<usize> = None;

    let width = lines[row].len();

    for x in col..width {
        let current = lines[row][x];
        if current == stop {
            break;
        }

        if current == target {
            result = Some(x);
        }
    }

    result
}

fn shift_top(lines: &mut Vec<Vec<char>>) {
    for row in 0..lines.len() {
        let width = lines[row].len();

        for col in 0..width {
            let c = lines[row][col];
            
            if c == 'O' {
                if let Some(newrow) = find_top(&lines, row, col, '.')  {
                    lines[newrow][col] = 'O';
                    lines[row][col] = '.';
                }
            }
        }
    }
}

fn shift_bot(lines: &mut Vec<Vec<char>>) {
    for row in 0..lines.len() {
        let width = lines[row].len();

        for col in 0..width {
            let c = lines[row][col];
            
            if c == 'O' {
                if let Some(newrow) = find_bot(&lines, row, col, '.')  {
                    lines[newrow][col] = 'O';
                    lines[row][col] = '.';
                }
            }
        }
    }
}

fn shift_left(lines: &mut Vec<Vec<char>>) {
    for row in 0..lines.len() {
        let width = lines[row].len();

        for col in 0..width {
            let c = lines[row][col];
            
            if c == 'O' {
                if let Some(newcol) = find_left(&lines, row, col, '.')  {
                    lines[row][newcol] = 'O';
                    lines[row][col] = '.';
                }
            }
        }
    }
}

fn shift_right(lines: &mut Vec<Vec<char>>) {
    for row in 0..lines.len() {
        let width = lines[row].len();

        for col in 0..width {
            let c = lines[row][col];
            
            if c == 'O' {
                if let Some(newcol) = find_right(&lines, row, col, '.')  {
                    lines[row][newcol] = 'O';
                    lines[row][col] = '.';
                }
            }
        }
    }
}

fn spin(lines: &mut Vec<Vec<char>>) {
    shift_top(lines);
    shift_left(lines);
    shift_bot(lines);
    shift_right(lines);
}

fn main() {
    let mut lines: Vec<Vec<char>> = std::io::stdin().lock().lines().map(|l| l.unwrap().chars().collect()).collect();

    shift_top(&mut lines);
    println!("part 1: {}", sumgrid(&lines));

    shift_left(&mut lines);
    shift_bot(&mut lines);
    shift_right(&mut lines);

    printlines(&lines);

    // let total = 3;
    let total = 1000000000;

    let mut configurations: HashMap<Vec<Vec<char>>, usize> = HashMap::new();

    let mut i = 1;
    while i < total {
        spin(&mut lines);

        let period = match configurations.entry(lines.clone()) {
            Entry::Occupied(o) => {
                let previous: usize = *o.get();
                let period = i - previous;
                Some(period)
            },
            Entry::Vacant(v) => {
                v.insert(i);
                None
            },
        };

        i += 1;

        if let Some(p) = period {
            let remaining = total - i;
            let skipped = (remaining / p) * p;

            if skipped != 0 {
                i += skipped;
                println!("skip to i = {}", i);
                continue;
            }
        }

        // if i % (total / 100) == 0 {
        //     println!("{}", i / (total / 100));
        // }
    }

    println!("part 2: {}", sumgrid(&lines));
}

fn sumgrid(lines: &Vec<Vec<char>>) -> usize {
    let mut sum = 0;

    for (row, weight) in lines.iter().zip((1..=lines.len()).rev()) {
        sum += weight * row.iter().filter(|c| **c == 'O').count();
    }

    sum
}

fn printlines(lines: &Vec<Vec<char>>) {
    for row in 0..lines.len() {
        let width = lines[row].len();

        for col in 0..width {
            print!("{}", lines[row][col]);
        }

        println!("")
    }
}
