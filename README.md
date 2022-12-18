
<!-- README.md is generated from README.Rmd. Please edit that file -->

# littleGames

<!-- badges: start -->
<!-- badges: end -->

Usually we use R programming for statistical analysis, for
classification algorithm, for data visualization. However, R is rarely
used to program games.The goal of litteGames is to provide access to
games in order to spend some fun times in RStudio. You can play maze, guess
number, and more games are coming.

## Installation

You can install the development version of littleGames like so:

``` r
install.packages("devtools")
devtools::install_github("kamanso/littleGames")
```

## Example
``` r
library(littleGames)

#maze game
maze(15,3)

#guess number game
guessNum(0,100)
```
