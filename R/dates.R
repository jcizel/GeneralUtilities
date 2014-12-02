convertToDate <- function(x){
    require(lubridate)

    x <- as.factor(x)
    levels <- levels(x)
    
    .converter <- function(.x){
        if (.x %like% "[0-9]+Q[0-9]+"){
            o <- .x %>>%
            stringr:::str_split('Q') %>>%
            (l ~ l[[1]])

            year <- o[[1]] %>>% as.numeric
            qrt <- o[[2]] %>>% as.numeric 

            date <- as.character(as.Date(paste0(year,'-',qrt*3,'-',1)) + months(1) - days(1))
        } else if (.x %like% "^[0-9]{4}$") {
            date <- paste0(.x, "-12-31")
        } else if (.x %like% "^[0-9]{4}-[0-9]+-[0-9]+$") {
            date <- .x
        } else {
            stop('Check date specification!')
            date <- NA
        }

        return(date)
    }

    levelsm <- Vectorize(.converter)(levels) 
    return(levelsm[x])
}
