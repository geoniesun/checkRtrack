
<!-- README.md is generated from README.Rmd. Please edit that file -->

# checkRtrack

<!-- badges: start -->
<!-- badges: end -->

R package to define track widths. It is created for the course
“Introduction to Programming with R”. For now, you can define the lowest
points along your track to then define the outer limits of the tracks.

## Installation

You can install checkRtrack with

``` r
# install.packages("devtools")
devtools::install_github("geoniesun/checkRtrack")

   library(ggplot2)
   library(sf)
   library(terra)
   library(qgisprocess)
   library(dplyr)
   library(ggnewscale)
   library(tidyterra)
   library(colorspace)
   library(ggspatial)
```

## Exemplary Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(checkRtrack)
#> This is version 0.0.0.9000 of checkRtrack
#> Here I can insert some more comments fpr the starting
## basic example code
```

## Roadmap

- [ ] Fix bug: when fade_scr not included, plot looks wrong
- [ ] Visualize the crossprofiles from the side. To see if there is
  actually a path
- [ ] Take out the fade score as necessary column
- [ ] Set more filter that determine a path
- [ ] Create more options to modify the users definition of the “path
  end” (for now its the steepest slope point)

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
