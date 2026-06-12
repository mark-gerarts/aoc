import std/sets
import std/sequtils
import std/sugar

type
    Pos = (int, int)
    Grid = HashSet[Pos]

proc parseGrid(filename: string): Grid =
    collect(initHashSet()):
        for row, line in lines(filename).toSeq():
            for col, symbol in line:
                if symbol == '@':
                    {(row, col)}

proc numNeighbors(grid: Grid, pos: Pos): int =
    result = -1

    for dx in -1..1:
        for dy in -1..1:
            if (pos[0] + dx, pos[1] + dy) in grid:
                result += 1

proc step(grid: Grid): Grid =
    grid.toSeq().filterIt(grid.numNeighbors(it) >= 4).toHashSet()

proc stepUntilStable(grid: Grid): Grid =
    let newGrid = grid.step()

    if newGrid.len() == grid.len():
        grid
    else:
        stepUntilStable(newGrid)

let grid = parseGrid("input/04.txt")

echo "Part 1: ", (grid.len() - grid.step().len())
echo "Part 2: ", (grid.len() - grid.stepUntilStable().len())
