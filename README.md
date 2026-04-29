# triang

R package implementing the triangular distribution.

## Description

This package provides functions for the triangular distribution:
- dtriang: density
- ptriang: distribution function
- qtriang: quantile function
- rtriang: random generation

## Installation

You can install the package from GitHub using:

```r
remotes::install_github("baicasTD/triang")
```
## Examples
```r
dtriang(3, 0, 10, 5)
ptriang(3, 0, 10, 5)
qtriang(0.5, 0, 10, 5)
rtriang(5, 0, 10, 5)
```
## Author
Francisco Javier Bailach Castilla
