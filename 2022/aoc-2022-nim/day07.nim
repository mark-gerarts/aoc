import os
import std/strscans
import std/strutils
import std/sequtils
import std/strformat

type

    InputKind = enum
        cd
        ls

    Input = object
        case kind: InputKind
        of cd:
            dir: string
        of ls:
            discard

    OutputKind = enum
        file
        dir

    Output = object
        name: string
        case kind: OutputKind
        of file:
            size: Natural
        of dir:
            discard

    TerminalLineKind = enum
        input
        output

    TerminalLine = object
        case kind: TerminalLineKind
        of input:
            input: Input
        of output:
            output: Output

func parseLine(line: string): TerminalLine =
    var (success, name) = line.scanTuple("$$ cd $*$.")
    if success:
        return TerminalLine(kind: input, input: Input(kind: cd, dir: name))

    (success, ) = line.scanTuple("$$ ls")
    if success:
        return TerminalLine(kind: input, input: Input(kind: ls))

    (success, name) = line.scanTuple("dir $w")
    if success:
        return TerminalLine(kind: output, output: Output(kind: dir, name: name))

    var size: int
    (success, size, name) = line.scanTuple("$i $*$.")
    if success:
        return TerminalLine(kind: output, output: Output(kind: file, size: size, name: name))

    raise newException(Exception, "No match")

type
    FileItemKind = enum
        file
        dir

    FileItem = ref object
        name: string
        case kind: FileItemKind
        of file:
            size: Natural
        of dir:
            children: seq[FileItem] = @[]

func newFile(name: string, size: Natural): FileItem =
    FileItem(kind: file, name: name, size: size)

func newDir(name: string): FileItem =
    FileItem(kind: dir, name: name)

func addChild(item: FileItem, newItem: FileItem) =
    case item.kind
    of file:
        raise newException(Exception, "Can't add item to file")
    of dir:
        item.children.add(newItem)

func getChild(item: FileItem, name: string): FileItem =
    if item.kind == dir:
        for child in item.children:
            if child.name == name:
                return child

    return nil

func hasChild(item: FileItem, name: string): bool =
    return not isNil(item.getChild(name))

proc `$`(item: FileItem): string =
    proc toString(item: FileItem, depth: Natural): string =
        var output = repeat("  ", depth) & item.name

        if item.kind == file:
            return output

        for child in item.children:
            output = output & "\n" & toString(child, depth + 1)

        return output

    toString(item, 0)

proc parseDirectoryTree(): FileItem =
    let terminalLines = readFile(paramStr(1))
        .strip()
        .split("\n")
        .map(parseLine)

    var root = newDir("/")
    var stack = @[root]

    for terminalLine in terminalLines[1..^1]:
        case terminalLine.kind
        of input:
            case terminalLine.input.kind
            of ls:
                discard
            of cd:
                let dir = terminalLine.input.dir

                if dir == "..":
                    discard stack.pop()
                else:
                    stack.add(stack[^1].getChild(dir))
        of output:
            case terminalLine.output.kind
            of dir:
                let dir = terminalLine.output.name
                if not stack[^1].hasChild(dir):
                    stack[^1].addChild(newDir(dir))
            of file:
                let name = terminalLine.output.name
                let size = terminalLine.output.size
                stack[^1].addChild(newFile(name, size))

    return root

func getSize(item: FileItem): int =
    if item.kind == file:
        return item.size

    for child in item.children:
        result += child.getSize()

func part1(root: FileItem): int =
    func recurse(item: FileItem, totalSize: int): int =
        var size = totalSize

        case item.kind
        of file:
            return 0
        of dir:
            let dirSize = item.getSize()
            if dirSize <= 1_00_000:
                size += dirSize

        for child in root.children:
            recurse(child, totalSize)



let rootDir = parseDirectoryTree()
echo $rootDir

echo "Part 1: " & $part1(rootDir)
