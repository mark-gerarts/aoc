# AoC 2025 in Prism

Advent of Code 2025 in Stephen Diel's language
[Prism](https://github.com/sdiehl/prism) (v0.15.0).

Easiest usage is through the Docker image:

```sh
docker run \
  -v $PWD:/aoc \
  -e PRISM_STORE_PATH=/aoc/.prism \
  -w /aoc \
  ghcr.io/sdiehl/prism run 01.pr
```

This assumes the puzzle inputs are present in the `input` folder, e.g.,
`input/01.txt`.

## Remarks on Prism

Some thoughts on the language, in no particular order.

- Some stuff is still undocumented (e.g. `parse_int`)
- String and char functions could be nicer (indexing, pattern matching)
- List.scan_left and Seq.scan have different orders
- Pipe `|>` doesn't allow placeholders, e.g. `1 |> myfn(_, 2)`.
- Compiler errors wrt inferred types kinda suck, e.g. 
  `type mismatch: expected List(?1245), got (List(Int)) -> List(Int) ! {?r1246}`
- Fewer parentheses would be nice, à la F# (e.g. can leave it for tuples,
  functions, etc.)
- Can use destructuring on a tuple in let and fn parameter, but not in for loop
- The built-in formatter is way too claustrophobic; it removes all whitespace
  that otherwise allows the code to breathe.
