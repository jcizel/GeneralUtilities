##' This function winsorizes an input numeric vector
##'
##' .. TODO ..
##' @title Winsorize a vector
##' @param x a numeric vector
##' @param method method of winsorization. 
##' @param k only applicable if method=="IQR". Specifies a number of
##' interquartile ranges from each quartile that define the winsorization limits.
##' @param fraction only applicable if method=="PERC"
##' @param na.rm omit missing values? Default is TRUE.
##' @param trim Trim extreme values rather setting them to a limit values?
##' Default is FALSE
##' @return winsorized numeric vector
##' @author Janko Cizel
winsorize <- function(
    x,
    method = c("IQR","PERC"),
    k=2,
    fraction = 0.01,
    na.rm = TRUE,
    trim = FALSE)
{
    if (is.numeric(x)){
        if (method == "IQR"){
            qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
            H <- k * IQR(x, na.rm = na.rm)
            if (trim == FALSE){
                x[x < (qnt[1] - H)] <- (qnt[1] - H)
                x[x > (qnt[2] + H)] <- (qnt[2] + H)
            } else {
                x[x < (qnt[1] - H)] <- NA
                x[x > (qnt[2] + H)] <- NA
            }
            return(as.numeric(x))
        } else if (method == "PERC"){
            lim <- quantile(x, probs=c(fraction, 1-fraction), na.rm = na.rm)
            if (trim == FALSE){
                x[ x < lim[1] ] <- lim[1]
                x[ x > lim[2] ] <- lim[2]
            } else {
                x[ x < lim[1] ] <- lim[1]
                x[ x > lim[2] ] <- lim[2]
            }
            return(as.numeric(x))
        } else {
            stop("Specify a winsorization method!")
        }
    } else {
        stop("x must be a numeric vector!")
    }
}
