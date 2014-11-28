orderElements <- function(x,
                          vars,
                          position = c('first','last')){
    out <- 
        switch(position,
               'first' = c(vars, setdiff(x,vars)),
               'last' = c(setdiff(x,vars),vars)
               )
    return(out)
}


## orderElements(x = c('zz','zzz','a','b','c','d'),
##               vars = c('zz','zzz'),
##               position = 'last')

## orderElements(x = c('zz','zzz','a','b','c','d'),
##               vars = c('zz','zzz'),
##               position = 'first')
