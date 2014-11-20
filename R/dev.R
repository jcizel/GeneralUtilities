##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title List all (public and private) functions within the specified package
##' @param pkg character vector with the name of the package
##' @return character string with names of functions within the package.
##' @author Janko Cizel
listAllFun <- function(pkg = 'data.table'){
    ## http://stackoverflow.com/questions/8696158/find-all-functions-including-private-in-a-package
    return(ls(getNamespace(pkg), all.names=TRUE))
}

## search()
## listAllFun(pkg = 'ggplot2')
## listAllFun(pkg = 'devtools')
