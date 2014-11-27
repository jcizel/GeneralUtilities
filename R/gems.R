modify <- function(x,
                   operations =
                       c('shift(lag = -1, dif = TRUE)',
                         '+1')){
    if (!is.numeric(x)) stop('`x` must be a numeric vector!')
    
    q <- parse(text = paste(c("x",operations), collapse = "%>>%"))
    return(eval(q))
}

## x <- c(1,2,3,1,2,3)
## modify(x,
##        operations =
##        c('`+`(1)',
##          '`+`(2)',
##          '`*`(2)'))
