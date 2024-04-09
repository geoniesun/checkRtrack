
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

``` r
#library(checkRtrack)
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
