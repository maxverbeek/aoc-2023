import fs from "fs";

const stdin = fs.readFileSync(0);
const lines = stdin.toString().trim().split("\n");

const heatmap: number[][] = lines.map((line: string) =>
  line.split("").map((c) => parseInt(c))
);

const width = heatmap[0].length;
const height = heatmap.length;

function inbounds(row: number, col: number): boolean {
  return row >= 0 && row < height && col >= 0 && col < width;
}

enum Direction {
  North = 0,
  East,
  South,
  West,
}

const alldirections = [
  Direction.North,
  Direction.East,
  Direction.South,
  Direction.West,
];

function rowOffset(dir: Direction): number {
  switch (dir) {
    case Direction.North:
      return -1;
    case Direction.South:
      return 1;
    default:
      return 0;
  }
}

function colOffset(dir: Direction): number {
  switch (dir) {
    case Direction.East:
      return -1;
    case Direction.West:
      return 1;
    default:
      return 0;
  }
}

function ishorizontal(dir: Direction): boolean {
  switch (dir) {
    case Direction.North:
    case Direction.South:
      return false;
    default:
      return true;
  }
}

// an entry of the priority queue
type Entry = {
  row: number;
  col: number;
  dirdistance: DirDistance;
};

// an entry that defines how far we've walked, and whether we arrived here in a horizontal direction
type DirDistance = {
  distance: number;
  horizontal: boolean;
};

function insert(queue: Entry[], entry: Entry) {
  let idx = queue.length - 1;

  // search from the back of the queue to the last element that has greater distance.
  // it's more likely this happens at the back.
  while (
    idx > 0 &&
    queue[idx].dirdistance.distance > entry.dirdistance.distance
  ) {
    idx = idx - 1;
  }

  // insert element after this one
  queue.splice(idx + 1, 0, entry);
}

function walk(
  heatmap: number[][],
  visited: DirDistance[][][],
  queue: Entry[],
  makeChildren: GenerateFunc
) {
  const {
    row,
    col,
    dirdistance: { distance, horizontal },
  } = queue.shift()!;

  // if a previous visit with the same "horizontality" but a shorter distance was found, dont bother
  // with this iteration
  const betterVisits = (visited[row][col] ?? [])
    .filter((v) => v.horizontal === horizontal)
    .filter((v) => v.distance <= distance);

  if (betterVisits.length > 0) {
    return;
  }

  visited[row][col].push({
    distance: distance,
    horizontal: horizontal,
  });

  for (const d of alldirections) {
    // if the way we arrived here was horizontal, and the direction we want to go in is also
    // horizontal, do not go in that direction. idem for vertical
    if (ishorizontal(d) === horizontal) {
      continue;
    }

    const children = makeChildren(heatmap, row, col, d);

    children.forEach((c) =>
      insert(queue, {
        row: c.row,
        col: c.col,
        dirdistance: {
          horizontal: c.dirdistance.horizontal,
          distance: distance + c.dirdistance.distance,
        },
      })
    );
  }
}

type GenerateFunc = (
  heatmap: number[][],
  row: number,
  col: number,
  dir: Direction
) => Entry[];

function generatePart1(
  heatmap: number[][],
  row: number,
  col: number,
  dir: Direction
) {
  let newdist = 0;

  const drow = rowOffset(dir);
  const dcol = colOffset(dir);
  const horizontal = ishorizontal(dir);

  let result: Entry[] = [];

  for (let i = 1; i <= 3; i++) {
    const nrow = row + drow * i;
    const ncol = col + dcol * i;

    if (inbounds(nrow, ncol)) {
      newdist += heatmap[nrow][ncol];

      result.push({
        row: nrow,
        col: ncol,
        dirdistance: {
          distance: newdist,
          horizontal: horizontal,
        },
      });
    }
  }

  return result;
}

function generatePart2(
  heatmap: number[][],
  row: number,
  col: number,
  dir: Direction
) {
  let newdist = 0;

  const drow = rowOffset(dir);
  const dcol = colOffset(dir);
  const horizontal = ishorizontal(dir);

  let result: Entry[] = [];

  for (let i = 1; i <= 10; i++) {
    const nrow = row + drow * i;
    const ncol = col + dcol * i;

    if (inbounds(nrow, ncol)) {
      newdist += heatmap[nrow][ncol];

      if (i < 4) {
        continue;
      }

      result.push({
        row: nrow,
        col: ncol,
        dirdistance: {
          distance: newdist,
          horizontal: horizontal,
        },
      });
    }
  }

  return result;
}

function solve(generate: GenerateFunc): number {
  const visited = new Array(height);

  for (let i = 0; i < height; i++) {
    visited[i] = new Array(width);

    for (let j = 0; j < width; j++) {
      visited[i][j] = [];
    }
  }

  const queue: Entry[] = [
    {
      row: 0,
      col: 0,
      dirdistance: {
        distance: 0,
        horizontal: true,
      },
    },
    {
      row: 0,
      col: 0,
      dirdistance: {
        distance: 0,
        horizontal: false,
      },
    },
  ];

  const drow = height - 1;
  const dcol = width - 1;

  while (queue.length) {
    const head = queue[0];
    if (head.row === drow && head.col === dcol) {
      console.log("distance: ", head.dirdistance.distance);
      return head.dirdistance.distance;
    }

    walk(heatmap, visited, queue, generate);
  }

  return -1;
}

solve(generatePart1);
solve(generatePart2);
