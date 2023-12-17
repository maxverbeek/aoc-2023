import javax.swing.plaf.basic.BasicInternalFrameTitlePane;
import java.util.ArrayList;
import java.util.OptionalInt;
import java.util.Scanner;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.lang.Math.max;

enum Direction {
    NORTH(0),
    EAST(1),
    SOUTH(2),
    WEST(3);
    public final int bitval;

    Direction(int bitval) {
        this.bitval = bitval;
    }

    Integer bitmapAdd(Integer bitmap) {
        return bitmap | (1 << this.bitval);
    }

    boolean isIn(Integer bitmap) {
        return (bitmap & (1 << this.bitval)) > 0;
    }

    Direction rotateLeft() {
        switch (this) {
            case NORTH: return WEST;
            case EAST: return NORTH;
            case SOUTH: return EAST;
            case WEST: return SOUTH;
            default: throw new RuntimeException("no direction??");
        }
    }
    Direction rotateRight() {
        switch (this) {
            case NORTH: return EAST;
            case EAST: return SOUTH;
            case SOUTH: return WEST;
            case WEST: return NORTH;
            default: throw new RuntimeException("no direction??");
        }
    }

    int rowOffset() {
        switch (this) {
            case NORTH: return -1;
            case EAST:
            case WEST: return 0;
            case SOUTH: return 1;
            default: throw new RuntimeException("no row offset");
        }
    }

    int colOffset() {
        switch (this) {
            case NORTH:
            case SOUTH: return 0;
            case EAST: return 1;
            case WEST: return -1;
            default: throw new RuntimeException("no col offset");
        }
    }
}

public class Main {
    public static void main(String[] args) {
        Scanner stdin = new Scanner(System.in);
        ArrayList<String> lines = new ArrayList<String>();

        while (stdin.hasNext()) {
            lines.add(stdin.nextLine());
        }

        System.out.println("part 1: " + countvisited(lines, 0, 0, Direction.EAST));

        final int height = lines.size();
        final int width = lines.get(0).length();

        // left -> right
        final int tophoriz = IntStream.range(0, width).map(x -> countvisited(lines, 0, x, Direction.SOUTH)).max().orElseThrow();
        final int bothoriz = IntStream.range(0, width).map(x -> countvisited(lines, height - 1, x, Direction.NORTH)).max().orElseThrow();

        // top -> bottom
        final int ltopdown = IntStream.range(0, height).map(y -> countvisited(lines, 0, y, Direction.EAST)).max().orElseThrow();
        final int rtopdown = IntStream.range(0, height).map(y -> countvisited(lines, width - 1, y, Direction.WEST)).max().orElseThrow();

        final int largest = max(tophoriz, max(bothoriz, max(ltopdown, rtopdown)));

        System.out.println("part 2: " + largest);
    }

    public static int countvisited(ArrayList<String> lines, int row, int col, final Direction dir) {
        final int height = lines.size();
        final int width = lines.get(0).length();

        int[][] seenmemo = new int[height][width];

        visit(lines, row, col, dir, seenmemo);

        int sum = 0;

        for (int[] rows: seenmemo) {
            for (int seendirs : rows) {
                if (seendirs != 0) {
                    sum += 1;
                }
            }
        }

        return sum;
    }

    public static void visit(ArrayList<String> lines, int row, int col, final Direction dir, int[][] seen) {
        final int height = lines.size();
        final int width = lines.get(0).length();

        if (row < 0 || row >= height || col < 0 || col >= width) {
            return;
        }

        if (dir.isIn(seen[row][col])) {
            return;
        }

        seen[row][col] = dir.bitmapAdd(seen[row][col]);

        final char curchar = lines.get(row).charAt(col);
        final boolean horizontal = dir == Direction.EAST || dir == Direction.WEST;
        final boolean vertical = !horizontal;

        if (curchar == '.' || (horizontal && curchar == '-') || (vertical && curchar == '|')) {
            visit(lines, row + dir.rowOffset(), col + dir.colOffset(), dir, seen);
            return;
        }

        if (curchar == '/' || curchar == '\\') {
            Direction newdir;
            if (horizontal == (curchar == '/')) {
                newdir = dir.rotateLeft();
            } else {
                newdir = dir.rotateRight();
            }

            visit(lines, row + newdir.rowOffset(), col + newdir.colOffset(), newdir, seen);
            return;
        }

        if ((horizontal && curchar == '|') || (vertical && curchar == '-')) {
            Direction dir1 = dir.rotateLeft();
            Direction dir2 = dir.rotateRight();

            visit(lines, row + dir1.rowOffset(), col + dir1.colOffset(), dir1, seen);
            visit(lines, row + dir2.rowOffset(), col + dir2.colOffset(), dir2, seen);
        }
    }
}