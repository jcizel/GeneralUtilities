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


shiftData <- function(
    data,
    var,
    by = NULL,
    lag = -1,
    dif = FALSE,
    relative = FALSE
){
    dt <- copy(data)
    
    for (x in var){
        .n <- paste0(x,
                     ".",
                     ifelse(sign(lag)<0,'L','F'),
                     abs(lag),
                     ifelse(dif == TRUE, '.DIF',''),
                     ifelse(relative == TRUE, '.REL',''))
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
