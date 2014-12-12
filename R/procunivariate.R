.summarizeCharacter <- function(x){
    if (!is.character(x) & !is.factor(x)) stop('x must be a character or factor vector!')
    
    if (is.factor(x)){
        x <- as.character(x)
    }

    .c <- .countObs(x)
    
    table(x) %>>%
    as.list %>>%
    list.sort(desc(.)) %>>%
    list.map(sprintf("%s: %.0f",.name,as.numeric(.))) %>>%
    list.take(n = 5) %>>%
    unlist %>>%
    paste(collapse = "; ") %>>%
    (~ .o)

    .s <-
        list(TOP5 = .o)

    out <- 
        c(.c,.s) 

    return(out)
}

.preprocessNumeric <- function(x){
    if (!is.numeric(x)) stop('x must be a numeric vector!')

    x[is.infinite(x)] <- NA
    
    return(x)
}

.countObs <- function(x){
    if (is.numeric(x)){
        out <- 
            list(N    = length(x),
                 NOBS = length(x[!is.na(x)]))
    } else {
        out <-
            list(N    = length(x),
                 NOBS = length(x[!is.na(x) & (x!="")]))
    }
    return(out)
}

.summarizeNumeric <- function(x){
    if (!is.numeric(x)) stop('x must be a numeric vector!')

    x <- .preprocessNumeric(x)

    .c <- .countObs(x)

    .s <-
        list(MEAN = mean(x, na.rm = TRUE),
             SD   = sd(x, na.rm = TRUE))
    
    .q <- quantile(x,
                   probs = seq(0,1,0.1),
                   na.rm = TRUE) %>>% as.list

    out <- 
        c(.c,.s,.q) 

    return(out)    
}


procSummarize <- function(data,
                          drop = 'IDNR'){
    if (!inherits(data,"data.table")) stop('Input data must be of `data.table` class!')

    varsel <- setdiff(names(data),
                      drop)

    out <- list()
    for (x in varsel){
        cat(sprintf('Processing: %s.\n',x))

        if (is.character(data[[x]]) | is.factor(data[[x]])){
            o <- .summarizeCharacter(data[[x]])
        } else {
            o <- .summarizeNumeric(data[[x]])
        }
        out[[x]] <- o
    }


    out %>>%
    list.map(c(NAME = .name, .) %>>% as.data.table) %>>%
    rbindlist(fill = TRUE) %>>%
    (~ result)

    setcolorder(result, c(setdiff(names(result),"TOP5"),"TOP5"))

    cat(sprintf("NAME=%.4s   NOBS=%13.0f  MIN=%13.0f MED=%13.0f MAX=%13.0f\n",
                result$NAME,
                result$NOBS,
                result$`0%`,
                result$`50%`,                
                result$`100%`))

    return(result)
}

## r <- 
##     procSummarize(data = dt)
