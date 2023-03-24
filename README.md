# data.table.extras

A personal R package, which facilitates common operations performed with the [`data.table`](https://github.com/Rdatatable/data.table) package.

## Installation

If you are interested in using the package, feel free to download it from Github with:

``` r
# install.packages("devtools")
devtools::install_github("DavidJesse21/data.table.extras")
```

## Implemented Functions

I do not pursue a clearly defined goal with this package.
Instead, one can rather consider it as an evolving collection of functions for data manipulation routines that I frequently encounter.<br>
I would consider the `setj_` and `dropj_` family of functions as the ones, that could be the most useful for other users as well.
They are inspired by the old [`dplyr`](https://github.com/tidyverse/dplyr) `mutate_` functions (e.g. `mutate_if()` or `mutate_at()`), but are adapted to the philosophy and functionality of `data.table` (reference semantics).<br>
For a detailed description of these and the few other functions from this package, please refer to the package's manual.

