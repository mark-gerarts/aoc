# Advent of Code 2025 - Common Lisp

## Setup

Dependencies are managed with [ocicl](https://github.com/ocicl/ocicl). Run
`ocicl install` to fetch them. Alternatively, check the ASD definition and
`quickload` the required libraries manually.

Input is expected to be in the `input` folder as `input/01.txt`, `input/02.txt`,
etc.

Optional: I ran the following command to register this project with asdf
(removing the need to call `defsystem` manually each time):

```sh
ln -s $PWD ~/.local/share/common-lisp/aoc-2025-cl
```

## Usage

I use emacs to develop. Typically I do:

- `M-x slime-load-system RET aoc-2025-cl RET`
- Open up a day and press `C-c C-k` and maybe `C-c ~`

Note that this requires `slime-contribs`/`slime-asdf`.

To run a day from the commandline (with SBCL):

```sh
sbcl --noinform --quit --eval "(asdf:load-system :aoc-2025-cl)" --load 01.lisp
```
