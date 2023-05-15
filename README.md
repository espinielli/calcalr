
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calcalr

**WORK in PROGRESS**

<!-- badges: start -->
<!-- badges: end -->

The calcalr package implements the algorithms of the book

> E. M. Reingold, N. Dershowitz “*Calendrical Calculations - The
> Ultimate Edition*”, Cambridge University Press, April 2018

## Concepts

The easiest way to reckon time is simply to count days. Reingold and
Dershowitz have chosen midnight at the onset of Monday, January 1, 1
(Gregorian) as the fixed date 1, which is abbreviate as R.D.[^1] 1.

An R.D. that has a fractional part giving the time of day is called a
“*moment*”. So noon on day *i* would be specified by $i + 0.5$.

All calendars provide conversion functions to/from R.D. making it
possible to convert from any one calendar to any other one.

## Installation

You can install the development version of calcalr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("espinielli/calcalr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(calcalr)
## basic example code
```

[^1]: R.D. stands for *Rata Die*, or fixed date in Latin.
