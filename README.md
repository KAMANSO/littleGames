
<!-- README.md is generated from README.Rmd. Please edit that file -->

# littleGames

<!-- badges: start -->
<!-- badges: end -->

Usually we use R programming for statistical analysis, for
classification algorithm, for data visualization.However, R is rarely used to program games.The goal of litteGames is to provide access to games in order to spend some fun times in RStudio. You can play maze, guess number, and more games are coming.

## Installation

To install the littleGames demonstration package, you will need to install the release version of devtools from CRAN with
```{r}
install.packages("devtools")
devtools::install_github("kamanso/littleGames")
```

## Example
This is a basic example which shows you how to solve a common problem:

``` r
library(littleGames)

#maze game
maze(10,3)

#guess number game
guessNum(0,100)
#then type any number and double click enter
```
