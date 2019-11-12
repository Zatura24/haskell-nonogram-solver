# haskell-nonogram-solver
A nonogram solver written in Haskell

## Starting the application
Make sure [`GHCi`](https://www.haskell.org/downloads/) is installed.

1. Open a terminal
2. Clone this repository `git clone [url]`
3. Change the current working directory to the cloned application
4. Run `ghci solver.hs`
5. Once inside `ghci`, type `main`

That's all!

## Changing nonograms
Inside the solver.hs file are preloaded a few different nonograms. To load one of these, change to a different `main` line. After that, reload the `solver.hs` file with `:r` inside `ghci` or restarting `ghci`.

Make sure only one `main` line is pressent at all times.

To add a new nonogram, create a new main line, as follows:  
```haskell
example:
    |   | 1 |   |
    | 2 | 1 | 2 |
  1 |   | x |   |
1 1 | x |   | x |
  3 | x | x | x |

main = putStr $ nonogram [[1],[1,1],[3]] [[2],[1,1],[2]]
```
The first input is a list with all the row information values from left to right, top to bottom. The seccond input is a list with all the column information values from top to bottom, left to right.