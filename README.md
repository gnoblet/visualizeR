
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visualizeR <img src="man/figures/logo.png" align="right" alt="" width="120"/>

> What a color! What a viz!

`visualizeR` proposes some utils to get REACH and AGORA colors,
ready-to-go color palettes, and a few visualization functions
(horizontal hist graph for instance).

## Installation

You can install the last version of visualizeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gnoblet/visualizeR", build_vignettes = TRUE)
```

## Roadmap

Roadmap is as follows:

-   [ ] Add IMPACT’s colors
-   [ ] Add all color palettes from the internal documentation
-   [ ] Add new types of visualization (e.g. dumbbell plot)
-   [ ] Use examples
-   [ ] Add some ease-map functions
-   [ ] Add some interactive functions (maps and graphs)

## Request

Please, do not hesitate to pull request any new viz or colors or color
palettes, or to email request any change
(<guillaume.noblet@reach-initiative.org> or <gnoblet@zaclys.net>).

## Example

``` r
library(visualizeR)
# Get all saved REACH colors, named
cols_reach(unnamed = F)
```
