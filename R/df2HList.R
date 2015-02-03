##' Recursively partition the data.frame along the specified dimensions
##'
##' This function partitions a data.frame `dt` by splitting ita long dimensions
##' specified in character vector of variable names `split`. The final result
##' consists of a nested list of children nodes.
##
##' @title Recursive partitioning of a data.frame 
##' @param dt data.frame or data.table
##' @param split character vector specifying the names of variables in `dt`, along
##' which the `dt` should be partitioned.
##' @param sizeVar 
##' @return nested list of children nodes
##' @author Janko Cizel
df2HList <- function(
    dt = NULL,
    split = NULL,
    sizeVar = NULL
){
    ## Sanitary checks
    if (!inherits(dt,'data.frame'))
        stop("`dt` must be a data frame")
    if (is.null(split))
        stop("You must provide a name of at least one column that splits the `dt`.")
    if ( ! lapply(split,  function(s) s %in% names(dt)) %>>% unlist %>>% all )
        stop('One of the names in `split` is not present in `dt`.')
    if ((sizeVar %in% split)|!(sizeVar %in% names(dt)))
        stop("sizeVar must be present in `dt` and must not be contained in `split`.")

    ## Once split-up of `dt` is completed, do the following
    if (length(split) == 0){
        dt %>>%
        apply(
            1,
            function(r){
                as.list(r)
            }
        ) %>>%
        list.map(
            list(
                name = .name,
                size =  get(sizeVar)
            )
        ) ->
            fin

        names(fin) <- NULL

        return(fin)
    }    

    ## Begin by splitting `dt`
    x = split[1]
    split = split[-1]

    dt %>>%
    split(
        .[[x]]
    ) %>>%
    list.map(
        list(
            name = sprintf("%s: %s", x, .name),
            children = df2HList(
                dt = .,
                split = split,
                sizeVar = sizeVar
            )
        )
    ) ->
        o

    names(o) <- NULL
    
    return(o)
}


## ## require(pipeR)
## ## require(rlist)


## ## HIERARCHICAL PLOTS WITH `networkD3`
## ##
## ## Below I illustrate the utility of the `df2Hlist` function by recreating some
## ## of the examples discussed by `@timelyportfolio` in Week 3 of his `htmlwidgets`
## ## blog (see http://www.buildingwidgets.com/blog/).

## devtools::install_github("timelyportfolio/networkD3@feature/d3.chart.layout")
## require(networkD3)

## l <- 
##     df2HList(dt = mtcars,
##              split = c('gear','carb'),
##              sizeVar = 'wt')

## ## Add root node (necessary for some graphs)
## l2 <- list(
##     root = 'test',
##     children = l
## )

## ## Plot 1
## hierNetwork(
##     List = l2,
##     type = 'tree.cartesian',
##     zoomable = T,
##     collapsible = T
## )

## ## Plot 2
## hierNetwork(
##     List = l2,
##     type = 'tree.radial',
##     zoomable = T,
##     collapsible = T
## )

## ## The following plots also rely on the sizeArg argument

## ## Plot3
## hierNetwork(
##     List = l2,
##     type = 'pack.nested',
##     zoomable = T,
##     collapsible = T
## )

## ## Plot4
## hierNetwork(
##     List = l2,
##     type = 'pack.nested',
##     zoomable = T,
##     collapsible = T
## )


## ## Plot 5
## hierNetwork(
##     List = l2,
##     type = 'partition.rectangle',
##     zoomable = T,
##     collapsible = T
## )
