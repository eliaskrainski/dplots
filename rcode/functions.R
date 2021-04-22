ac12p <- function(x) {
    a <- rep(NA, length(x))
    for (j in 12:length(x))
        a[j] <- prod(1+x[-11:0+j])-1
    return(a)
}

