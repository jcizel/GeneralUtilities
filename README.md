# Utilities for Data Analysis in R

This R package contains a set of functions that facilitate commonly applied data
preparation and exploration steps.

### Main Functions in the Package
- *dt2HList*: sequentially splits a dataset along the pre-specified columns, and returns a
  result in a nested hierarchical list, which can be further used as an input in
  `networkD3` hierarchical plots (see examples below).
- *procUnivariate*: summarizes the dataset and stores the result in a data.frame
- *procExpand*: applies a selection of functions specified in a list to a
  pre-specified collections of variables.


### Installation
```R
devtools::install_github('jcizel/GeneralUtilities')
require(GeneralUtilities)
```

### Examples
#### `dt2HList`
```R
## HIERARCHICAL PLOTS WITH `networkD3`
##
## Below I illustrate the utility of the `df2Hlist` function by recreating some
## of the examples discussed by `@timelyportfolio` in Week 3 of his `htmlwidgets`
## blog (see http://www.buildingwidgets.com/blog/).

devtools::install_github("timelyportfolio/networkD3@feature/d3.chart.layout")
require(networkD3)

l <- 
    df2HList(dt = mtcars,
             split = c('gear','carb'),
             sizeVar = 'wt')

## Add root node (necessary for some graphs)
l2 <- list(
    root = 'test',
    children = l
)

## Plot 1
hierNetwork(
    List = l2,
    type = 'tree.cartesian',
    zoomable = T,
    collapsible = T
)

## Plot 2
hierNetwork(
    List = l2,
    type = 'tree.radial',
    zoomable = T,
    collapsible = T
)

## The following plots also rely on the sizeArg argument

## Plot3
hierNetwork(
    List = l2,
    type = 'pack.nested',
    zoomable = T,
    collapsible = T
)

## Plot4
hierNetwork(
    List = l2,
    type = 'pack.nested',
    zoomable = T,
    collapsible = T
)


## Plot 5
hierNetwork(
    List = l2,
    type = 'partition.rectangle',
    zoomable = T,
    collapsible = T
)

```
