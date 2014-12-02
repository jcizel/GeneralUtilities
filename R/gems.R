modify <- function(x,
                   operations =
                       c('shift(lag = -1, dif = TRUE)',
                         '(function(a,b) a^{1/b})(2)',
                         '+1')){
    if (!is.vector(x)) stop('`x` must be a vector!')
    
    ## q <- parse(text = paste(c("x",operations), collapse = "%>>%"))
    q <- parse(text = paste(c(substitute(x),operations), collapse = "%>>%"))
    return(eval(q, envir = parent.frame()))
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


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param data 
##' @param by 
##' @param keepvars 
##' @param dropvars 
##' @param convert specification of commands to be applied to predefined sets of
##' variables. The syntax is as follows
##' list('`var1,var2,var3,...`~`command1`;`command2`,...',...)
##' 
##' 
##'
##' @return 
##' @author Janko Cizel
procExpand <- function(
    data = mtcars %>>% data.table,
    by = 'gear',
    keepvars = NULL,
    dropvars = NULL,
    convert =
        list('_NUMERIC_ ~ `+`(1);`-`(2)',
             '_CHAR_ ~ nchar'),
    prefix = '',
    suffix = ''
){
    if (!inherits(data,'data.table')) stop('`data` must be a data.table!')
    if (by == '' || (keepvars %||% 'ok' == '')) stop("`by` and `keepvars` cannot be empty strings!")
    if (!is.null(keepvars) && !is.null(dropvars)) stop('`keepvars` and `dropvars` cannot both be specified simultaneously!')

    .convert <- .parseConvert(convert)

    if (is.null(by)){
        .convert %>>%
        list.update(vars = .varList(data,vars,drop = dropvars)) %>>%
        list.update(vars,vars = trim(stringr::str_split(vars,",")[[1]])) %>>%
        list.update(oper,oper = trim(stringr::str_split(oper,";")[[1]])) %>>%        
        (~ .c)
    }
    else{
        .convert %>>% list.update(vars =
                                      .varList(data,
                                               vars,
                                               drop = if (is.null(dropvars)) by else c(dropvars,by))) %>>%
        list.update(vars = .varList(data,vars,drop = dropvars)) %>>%
        list.update(vars,vars = stringr::str_split(vars,",")[[1]]) %>>%
        list.update(oper,oper = trim(stringr::str_split(oper,";")[[1]])) %>>%                
        (~ .c)
    }

    .c %>>%
    list.map(. %>>%  list.extract("vars")) %>>%
    unlist %>>%
    unique %>>%
    (x ~ x[x!=""]) %>>%
    (~ vars)    
    
    if (is.null(by)){
        keep <- c(keepvars,vars) %||% vars
        keep <- keep[keep != ""]
        dt <- copy(data[,.SD,.SDcols = c(keep)])
        for (x in vars){
            o <- rlist::list.find(.c, x %in% vars)[[1]][['oper']]
            ## dt[,(x) := modify(x = get(x), operations = o)]
            dt[,paste0(prefix,x,suffix) := modify(x = get(x), operations = o)]            
            setcolorder(dt, orderElements(names(dt),c(keepvars) %||% names(dt),'first'))
        }
    } else {
        keep <- c(keepvars,vars,by) %||% c(vars,by)
        keep <- keep[keep != ""]
        dt <- copy(data[,.SD,.SDcols = c(keep)])
        for (x in vars){
            o <- rlist::list.find(.c, x %in% vars)[[1]][['oper']]
            ## dt[,(x) := modify(x = get(x), operations = o)
            ##    , by = by]
            dt[,paste0(prefix,x,suffix) := modify(x = get(x), operations = o)
               , by = by]            
            setcolorder(dt, orderElements(names(dt),c(by,keepvars) %||% c(by),'first'))
        }        
    }
    return(dt)
}

## mtcars %>>% data.table %>>% (~ dt)
## by = 'gear'

## ## keepvars = c('hp','vs')
## ## convert <-
## ##     list('drat,wt'=c('shift(lag=-1,dif = TRUE,relative = TRUE)','`-`(1)'),
## ##          'qsec'=c('`+`(5)','`/`(10)'))


## by = 'gear'
## keepvars = 'cyl'
## convert =
##     list('_NUMERIC_ ~ shift(lag = -1, dif = TRUE)',
##          '_CHAR_ ~ nchar')
## convert =
##     list('__rat ~ `/`(cyl)',
##          '_CHAR_ ~ nchar')
## ## undebug(procExpand)
## ## undebug(modify)
## procExpand(
##     data = dt,
##     by = by,
##     keepvars = keepvars,
##     dropvars = NULL,
##     convert = convert,
##     prefix = '',
##     suffix = '_mod'
## )

.parseConvert <- function(convert =
                              list('var1,var2 ~ `+`1',
                                   'var3,var4 ~ `-`1')){
    p <- lapply(convert,function(x){trim(stringr::str_split(x,'~')[[1]])})

    convert %>>%
    list.map(. ~ trim(stringr::str_split(.,'~')[[1]])) %>>%
    (~ convert)
    
    o <- 
        foreach (x = 1:length(convert)) %do% {
            vars = stringr::str_split(string = p[[x]][[1]],',')[[1]]
            oper = p[[x]][[2]]
            list(vars = vars,
                 oper = oper)
        }
    return(o)
}

## .parseConvert(convert)


.varList <- function(
    data,
    varsel = "_NUMERIC_",
    drop = NULL
)
{
    .specials <- function(data = data,
                          varsel = varsel,
                          drop = drop){
        o <- {
            if (varsel == '_NUMERIC_')
                names(Filter(is.numeric, data))
            else if (varsel == '_CHAR_')
                names(Filter(is.character, data))
            else if (varsel %like% "^__"){
                pattern = gsub("(__)(.+)","\\2",varsel)
                o <- names(data)[names(data) %like% pattern]
                o
            } else {
                varsel
            }
        }

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
## (? dt ~ .varList(dt, varsel = '__rat')) %>>%
## (? dt ~ .varList(dt, varsel = c('x,y', '_NUMERIC_')))
