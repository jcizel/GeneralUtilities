modify <- function(x,
                   operations =
                       c('shift(lag = -1, dif = TRUE)',
                         '(function(a,b) a^{1/b})(2)',
                         '+1')){
    if (!is.vector(x)) stop('`x` must be a vector!')
    
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
    keepvars = NULL,
    convert =
        list('_NUMERIC_' = '`+`(1)',
             '_CHAR_' = 'nchar')
){
    if (!inherits(data,'data.table')) stop('`data` must be a data.table!')
    if (by == '' || keepvars == '') stop("`by` and `keepvars` cannot be empty strings!")

    if (is.null(by))
        names(convert) <- .varList(data, names(convert))
    else
        names(convert) <- .varList(data, names(convert),drop = by)
    
    .convert <- .parseConvert(convert)
    vars <- unique(unlist(lapply(.convert,function(x) x$vars)))
    vars <- vars[vars!=""]
    
    if (is.null(by)){
        keep <- c(keepvars,vars) %||% vars
        keep <- keep[keep != ""]
        dt <- copy(data[,.SD,.SDcols = c(keep)])
        for (x in vars){
            o <- rlist::list.find(.convert, x %in% vars)[[1]][['oper']]
            if (length(o)>1) stop('Only one stream of modifications can be applied to a single variable.')
            dt[,(x) := modify(x = get(x), operations = o)]
            setcolorder(dt, orderElements(names(dt),c(keepvars) %||% names(dt),'first'))
        }
    } else {
        keep <- c(keepvars,vars,by) %||% c(vars,by)
        keep <- keep[keep != ""]
        dt <- copy(data[,.SD,.SDcols = c(keep)])
        for (x in vars){
            o <- rlist::list.find(.convert, x %in% vars)[[1]][['oper']]
            ## if (length(o)>1) stop('Only one stream of modifications can be applied to a single variable.')
            dt[,(x) := modify(x = get(x), operations = o)
               , by = by]
            setcolorder(dt, orderElements(names(dt),c(by,keepvars) %||% c(by),'first'))
        }        
    }

    return(dt)
}

## mtcars %>>% data.table %>>% (~ dt)
## by = 'gear'
## keepvars = c('hp','vs')
## convert <-
##     list('drat,wt'=c('shift(lag=-1,dif = TRUE,relative = TRUE)','`-`(1)'),
##          'qsec'=c('`+`(5)','`/`(10)'))


## keepvars = NULL
## convert =
##     list('_NUMERIC_' = 'shift(lag = -3, dif = TRUE)',
##          '_CHAR_' = 'nchar')
## ## undebug(procExpand)
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
    
.varList <- function(
    data,
    varsel = "_NUMERIC_",
    drop  = NULL
)
{
    .specials <- function(data = data,
                          varsel = varsel,
                          drop = drop){
        o <- 
            switch(varsel,
                   '_NUMERIC_' = names(Filter(is.numeric, data)),
                   '_CHAR_' = names(Filter(is.character, data)),
                   varsel)

        if (!is.null(drop)) o <- setdiff(o,drop)

        out <- paste0(o, collapse = ',')
        return(out)
    }

    .fun <- Vectorize(.specials,vectorize.args = 'varsel')
    
    return(.fun(data = data,
                varsel = varsel,
                drop = drop) )
}

## mtcars %>>%
## (? dt ~ .varList(dt, varsel = '_NUMERIC_')) %>>%
## (? dt ~ .varList(dt, varsel = '_CHAR_')) %>>%
## (? dt ~ .varList(dt, varsel = c('x,y', '_NUMERIC_')))
