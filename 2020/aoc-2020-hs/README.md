# AdventOfCode 2020 in Haskell

I have almost no time this year, but let's just start off and see where (and
when) we end!

## Usage

Most of the time I'm in the ghci REPL already, so I just call the main method
from there:

```
$ stack ghci src/Day01A.hs
*Day01A> main
```

Otherwise it's just `stack runghc src/Day01A.hs`.

## Project structure

The project is started from the `simple` stack template, with
[ormolu](https://github.com/tweag/ormolu) as a linter. Each puzzle consists of 2
parts, which each get their own module. This is done because part 2 often leads
to a substantial refactor of part 1, and I want to keep this difference visible.
