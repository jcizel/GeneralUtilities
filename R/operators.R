##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Reproduce functionality of JS || operator. 
##' @param a any R object
##' @param b R object to be returned if a does not exists or is null
##' @return if a exists and is not null, return a, otherwise b.
##' @author Janko Cizel
`%||%` <- function(a,b){
    t <- try(a, silent = TRUE)
    if (("try-error" %in% class(t))) return(b)
    else {
        if (is.null(a)) return(b)
        else return(a)
    }
}

## rm(a)
## x <- 'test'
## a %||% x
## a <- NULL
## a %||% x
## a <- "hello"
## a %||% x
