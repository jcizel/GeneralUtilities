winsorize <- function(x,
                      method = "IQR",
                      k=2,
                      fraction = 0.01,
                      na.rm = TRUE, ...) {
    if (is.numeric(x) & method == "IQR"){
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- k * IQR(x, na.rm = na.rm)
        x[x < (qnt[1] - H)] <- (qnt[1] - H)
        x[x > (qnt[2] + H)] <- (qnt[2] + H)
        x
    } else if (is.numeric(x) & method == "PERC"){
        lim <- quantile(x, probs=c(fraction, 1-fraction), na.rm = na.rm)
        x[ x < lim[1] ] <- lim[1]
        x[ x > lim[2] ] <- lim[2]
        x
    }
    else{
        warning("The input is either not numeric or you supplied a faulty method of winsorization")
        as.factor(x)
    }
}
