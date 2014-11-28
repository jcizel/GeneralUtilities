modify <- function(x,
                   operations =
                       c('shift(lag = -1, dif = TRUE)',
                         '(function(a,b) a^{1/b})(2)',
                         '+1')){
    if (!is.numeric(x)) stop('`x` must be a numeric vector!')
    
    q <- parse(text = paste(c("x",operations), collapse = "%>>%"))
    return(eval(q))
}


## x <- c(1,2,3,1,2,3)
## modify(x,c('(function(x,y) x^y)(2)'))
## modify(x,
##        operations =
##        c('`+`(1)',
##          '`+`(2)',
##          '`*`(2)'))

## require(pipeR)
## iris %>>% data.table %>>% (~ iris)
## iris[, modify(Petal.Width,
##               operations =
##               c('shift(lag = -2,dif = TRUE,relative = TRUE)'))
##      , by = Species] %>>% data.frame


procExpand <- function(
    data = mtcars %>>% data.table,
    by = 'gear',
    keepvars = 'mpg',
    convert =
        list('cyl,disp' = '`+`(1)',
             'hp,drat' = '`-`(1)')
){
    if (!inherits(data,'data.table')) stop('`data` must be a data.table!')    
    .convert <- .parseConvert(convert)
    vars <- unique(unlist(lapply(.convert,function(x) x$vars)))       
    
    if (is.null(by)){
        keep <- c(keepvars,vars) %||% vars
        dt <- copy(data[,.SD,.SDcols = c(keep)])
        for (x in vars){
            o <- rlist::list.find(.convert, x %in% vars)[[1]][['oper']]
            if (length(o)>1) stop('Only one stream of modifications can be applied to a single variable.')
            dt[,(x) := modify(x = get(x), operations = o)]
        }
    } else {
        keep <- c(keepvars,vars,by) %||% c(vars,by)
        dt <- copy(data[,.SD,.SDcols = c(keep)])
        for (x in vars){
            o <- rlist::list.find(.convert, x %in% vars)[[1]][['oper']]
            if (length(o)>1) stop('Only one stream of modifications can be applied to a single variable.')
            dt[,(x) := modify(x = get(x), operations = o)
               , by = by]
        }        
    }
    return(dt)
}

## mtcars %>>% data.table %>>% (~ dt)
## by = 'gear'
## keepvars = c('hp','vs')
## convert <-
##     list('drat,wt'='shift(lag=-2,dif = TRUE)',
##          'qsec'='`+`(5)')

## procExpand(
##     data = dt,
##     by = by,
##     keepvars = keepvars,
##     convert = convert
## )

.parseConvert <- function(convert =
                              list('var1,var2' = '`+`1',
                                   'var3,var4' = '`-`1')){
    o <- 
        foreach (x = 1:length(convert)) %do% {
            vars = stringr::str_split(string = names(convert)[[x]],',')[[1]]
            oper = convert[[x]]
            list(vars = vars,
                 oper = oper)
        }
    return(o)
}
    
