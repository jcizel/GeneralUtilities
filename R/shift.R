##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Lag a time series within a panel 
##' @param x numeric vector to be shifted
##' @param shift.by number of legs
##' @param dif 
##' @param relative 
##' @return a vector 
##' @author Janko Cizel
shift<-function(x,
                lag = -1,
                dif = FALSE,
                relative = FALSE){
    stopifnot(is.numeric(lag))
    ## stopifnot(is.numeric(x))

    ## if (length(lag)>1)
    ##     return(lapply(lag,shift, x=x))

    out <- NULL
    abs.lag=abs(lag)

    if (length(x)<abs.lag){
        warning("Number of lags exceeds the length of the vector.")
        out <- rep(NA,length(x))
    } else {
        if (lag > 0 )
            out <- c(tail(x,-abs.lag),
                     rep(NA,abs.lag))
        else if (lag < 0 )
            out <- c(rep(NA,abs.lag),
                     head(x,-abs.lag))
    }
    
    if (dif == TRUE){
        if (relative ==TRUE){
            out <- (x - out)/out
        } else {
            out <- x - out            
        }
    }

    out[is.infinite(out)] <- NA
    
    return(out)
}

##' shiftData implements lead/lag/dif operations on a data.table and returns a
##' resulting dataset.
##'
##' .. content for \details{} ..
##' @title Lead/lag/dif operations on a data.table
##' @param data input dataset. Must be a data.table. 
##' @param var character vector with names of variables that should be modified
##' @param replace replace the original variables. If FALSE (by default), the
##' modified variables are added to the original dataset, with a suffix added
##' to the name of the original variables.
##' @param by character vector with name(s) of by groups.
##' @param lag number of lags (negative) or leads (positive). 
##' @param dif take a difference? Default is FALSE.
##' @param relative  relative differences? Applicable only if dif=TRUE.
##' @return data.table with the resulting dataset.
##' @author Janko Cizel
shiftData <- function(
    data,
    var,
    replace = FALSE,
    by = NULL,
    lag = -1,
    dif = FALSE,
    relative = FALSE
){
    if (!inherits(data, 'data.table')) stop('Data must be a data.table.')
    if (dif == FALSE) relative <- FALSE
    
    dt <- copy(data)
    
    for (x in var){
        if (replace == FALSE){
            .n <- paste0(x,
                         ".",
                         ifelse(sign(lag)<0,'L','F'),
                         abs(lag),
                         ifelse(dif == TRUE, '.DIF',''),
                         ifelse(relative == TRUE, '.REL',''))
        } else .n <- x
        
        if (!is.null(by)){            
            dt[, (.n) := {
                .x <- get(x)
                shift(.x,
                      lag = lag,
                      dif = dif,
                      relative = relative)
            }
             , by = by]
        } else {
            dt[, (.n) := {
                .x <- get(x)
                shift(.x,
                      lag = lag,
                      dif = dif,
                      relative = relative)
            }]            
        }
    }

    return(dt)
}

## shiftData(
##     data = data.table(iris),
##     var = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
##     replace = FALSE,
##     by = 'Species',
##     lag = -5,
##     dif = FALSE,
##     relative = FALSE
## )
