
## fix the data
fixDataFn <- function(x) {
    x <- gsub("Fev", "Feb", x)
    x <- gsub("Abr", "Apr", x)
    x <- gsub("Mai", "May", x)
    x <- gsub("Ago", "Aug", x)
    x <- gsub("Set", "Sep", x)
    x <- gsub("Out", "Oct", x)
    x <- gsub("Dez", "Dec", x)
    nch <- nchar(x)
    dspl <- sapply(1:length(x), function(i)
        substring(x[i], nch[i]-10))
    as.Date(dspl, format = "%d %b %Y")
}

predFn <- function(ydata, Dmax, jcol=1) {
    Date <- ydata[, jcol]
    ydata <- ydata[, setdiff(1:ncol(ydata),jcol), drop = FALSE]
    dmin <- min(Date)
    D0 <- rev(seq(Dmax, dmin - 1, -1))
    iday <- as.integer(difftime(Date, dmin, units = "day"))+1
    iday0 <- as.integer(difftime(D0, dmin, units = "day"))+1
    fits <- lapply(ydata, function(y)
        gam(y ~ s(i), data=data.frame(y=y,i=iday)))
    newd <- data.frame(i = iday0)
    preds <- lapply(fits, function(m) {
        prd <- predict.gam(m, newdata = newd,
                           type = "response", se.fit = TRUE)
        prd <- data.frame(Date = D0, fit = prd$fit, se.fit = prd$se.fit)
        prd$low <- prd$fit - 1.96 * prd$se.fit
        prd$upp <- prd$fit + 1.96 * prd$se.fit
        return(prd)
    })
    return(preds)
}
datFitPlotfn <- function(ydat, jcol=1, prd, ylim, xlim, col, fill, xlab, ylab) {
    Date <- ydat[, jcol]
    ydat <- ydat[, setdiff(1:ncol(ydat), jcol), drop = FALSE]
    if(missing(xlim) || is.null(xlim))
        xlim <- range(Date) + c(0, 10)
    if(missing(ylim) || is.null(ylim))
        ylim <- c(0, max(y, na.rm=TRUE))
    plot(Date, rep(50, length(Date)), type = "n",
         xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
    for(k in 1:length(prd)) {
        points(Date, ydat[, k], col = col[k], pch = 19)
        np <- length(prd[[k]]$fit)
        polygon(prd[[k]]$Date[c(1:np, np:1, 1)],
                c(prd[[k]]$low, rev(prd[[k]]$upp), prd[[k]]$low[1]),
                col = fill[k], border = 'transparent')
        lines(prd[[k]]$Date, prd[[k]]$fit,
              col = col[k], lty = 1, lwd = 2)
    }
}

### (old) alternative approaches
correctedBS2 <- function(x, knots) {
    bb <- bs(x = x, knots = knots, degree = 2)
    k <- ncol(bb) -2
    B <- bb[, 2:(k+1)]
    B[, 1] <- bb[, 1] + bb[, 2]
    B[, k] <- bb[, k+1] + bb[, k+2]
    colnames(B) <- paste0("B", 1:k)
    return(B)
}
basisPlot <- function(x, B) {
    plot(x, B[,1], type = 'n')
    for(k in 1:ncol(B))
        lines(x, B[,k])
}
predFn1 <- function(ydata, Dmax, h=21) {
    Date <- ydata[, ncol(ydata)]
    ydata <- ydata[, 1:(ncol(ydata)-1), drop = FALSE]
    dmin <- min(Date)
    D0 <- rev(seq(Dmax, dmin - 1, -1))
    iday <- as.integer(difftime(Date, dmin, units = "day"))+1
    iday0 <- as.integer(difftime(D0, dmin, units = "day"))+1
    bk0 <- rev(seq(max(iday0), -h/2, -h))
    B <- correctedBS2(x = iday0, knots = bk0)
    ff <- update(y ~ 1, paste(".~.+", paste0(colnames(B), collapse = "+")))
    fits <- lapply(ydata, function(y)
        lm(ff, data = data.frame(B[iday, ], y = y)))
    preds <- lapply(fits, function(m) {
        prd <- predict(m, newdata = as.data.frame(B),
                       type = "response", se.fit = TRUE)
        prd <- data.frame(Date = D0, fit = prd$fit, se.fit = prd$se.fit)
        prd$low <- prd$fit - 1.96 * prd$se.fit
        prd$upp <- prd$fit + 1.96 * prd$se.fit
        return(prd)
    })
    return(preds)
}
predFn2 <- function(ydata, Dmax) {
    Date <- ydata[, ncol(ydata)]
    ydata <- ydata[, 1:(ncol(ydata)-1), drop = FALSE]
    dmin <- min(Date)
    D0 <- rev(seq(Dmax, dmin - 1, -1))
    iday <- as.integer(difftime(Date, dmin, units = "day"))+1
    ivalues <- as.integer(difftime(D0, dmin, units = "day"))+1
    np <- length(ivalues)
    ff <- y ~ 0 + f(i, model = "rw2", values = ivalues, constr = FALSE)
    preds <- lapply(ydata, function(y) {
        dat <- data.frame(i = c(ivalues, iday),
                          y = c(rep(NA, np), y))
        r <- inla(ff, data = dat)
        fitt <- r$summary.fitted.values[1:np, ]
        prd <- data.frame(
            Date = D0, fit = fitt$mean,
            low = fitt[, 3]-1.96*fitt$sd,
            upp = fitt[, 5]+1.96*fitt$sd)
    })
    return(preds)
}
