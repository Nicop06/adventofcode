# Advent of code

This repository contains my solutions for the [advent of code
challenges](https://adventofcode.com). They are written in Haskell, with one
folder per year.

To install haskell, you can use [GHCup](https://www.haskell.org/ghcup/).

Then, you can go to the project for a given year, e.g. `cd 2025`. To run the
solution for `dayX`, you can run `stack run dayX`. You can also specify
multiple days, e.g. `stack run 1 3 {8..10}` to run days 1, 3, 8, 9, 10 or
`stack run {1..25}` to run all days (`stack run {1..12}` for 2025 as it only
has 12 days).
