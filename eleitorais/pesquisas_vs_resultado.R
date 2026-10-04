library(splines)

setwd(here::here("eleitorais"))

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
predFn <- function(ydata, Dmax) {
    Date <- ydata[, ncol(ydata)]
    ydata <- ydata[, 1:(ncol(ydata)-1), drop = FALSE]
    dmin <- min(Date)
    D0 <- rev(seq(Dmax, dmin - 1, -1))
    iday <- as.integer(difftime(Date, dmin, units = "day"))+1
    iday0 <- as.integer(difftime(D0, dmin, units = "day"))+1
    bk0 <- rev(seq(max(iday0), -5, -20))
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
datFitPlotfn <- function(ydat, prd, ylim, xlim, col, fill, xlab, ylab) {
    Date <- ydat[, ncol(ydat)]
    ydat <- ydat[, 1:(ncol(ydat)-1), drop = FALSE]
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

###########################################################################
### Data from pesquisas eleitorais em 2022
###########################################################################

fls22 <- paste0("pesquisas_", 1:2, "t.csv")
names(fls22) <- c("t1", "t2")
url22 <- paste0("https://raw.githubusercontent.com/Nexo-Dados/",
              "pesquisas-presidenciais-2022/main/")

for(i in 1:2)
    if(!file.exists(fls22[i]))
        download.file(paste0(url22, fls22[i]), fls22[i])

dat22a <- lapply(fls22, read.csv)
n22 <- sapply(dat22a, nrow)

head(dat22a[[1]],3)
head(dat22a[[2]],3)

dat22a[[1]]$"Outros + BNI" <- 100 - rowSums(dat22a[[1]][, 5:6])
summary(dat22a[[1]]$"Outros + BNI")
head(dat22a[[1]])

jj22 <- list(c(5:6,11), c(5,6,7))
dat22 <- list(t1 = dat22a[[1]][, jj22[[1]]],
              t2 = dat22a[[2]][, jj22[[2]]])
dat22[[1]]$Data <- as.Date(dat22a[[1]]$Data)
dat22[[2]]$Data <- as.Date(dat22a[[2]]$Data)

cores <- c("red", "green4", ##"orange", "blue", "brown", "gray",
           "black")

### RESULTADOS da eleicao em 2022
ntot22 <- c(123682372, 124252796)
nres22t1 <- c(Lula      = 57259504,
              Bolsonaro = 51072345,
              "Outros + BNI" =
                  sum(c(Ciro      =  3599287,
                        Tebet     =  4915423,
                        Outros    = 600955+559708+81129+53519+45620+25625+16604,
                        BNI       = 1964779 + 3487874)))
result22 <- list(
    "Turno 1" = 100 * (nres22t1/ntot22[1]),
    "Turno 2" = c(Lula =      100 * (60345999 / ntot22[2]),
                  Bolsonaro = 100 * (58206354 / ntot22[2]),
                  BNI = 1.43+3.16)
)
sapply(result22, sum)

res22 <- list(
    "Turno 1" = 100 * nres22t1[1:2]/(ntot22[1] - (1964779 + 3487874)),
    "Turno 2" = 100 * result22[[2]][1:2]/sum(result22[[2]][1:2]))
res22

Dmin22 <- as.Date(c("2022-01-01", "2022-08-15"))
Dmax22 <- as.Date(c("2022-10-02", "2022-10-30"))

Preds22 <- lapply(1:2, function(k)
    predFn(dat22[[k]], Dmax22[k]))

png("pesquisas2022.png", 3000, 2000, res = 300)
par(mfrow = c(2, 1), mar = c(3,3,0,0),
    mgp = c(2,1.0,0), bty = "n", las = 1)
for(k in 1:2) {
    datFitPlotfn(dat22[[k]], Preds22b[[k]], 
                 ylim = c(0,60), xlim = c(Dmin22[k], Dmax22[k]+20), 
                 col = cores, fill = c(rgb(1:0, c(.5,1), c(0,.5), .5), gray(.5,.5)),
                 xlab = "", ylab = "%")
    abline(h = 10*(0:5), lty = 2, col = gray(0.5))
    if(k==1)
        legend("top", names(result22[[k]]), bty = "n",
               ncol = 3, lty = 1, lwd = 2, col = cores)
    legend("topleft", "", bty = "n",
           title = paste("Pesquisas 2022 -", names(result22)[k]))
    legend("topright", "", title = paste("Resultado\nNom  Val"), bty = 'n')
    text(rep(Dmax22[k]+10, 3), result22[[k]],
         format(result22[[k]], digits = 4), col = cores)
    text(rep(Dmax22[k]+20, 2), res22[[k]],
         format(res22[[k]], digits = 4), col = cores[1:2])
}
dev.off()

system("eog pesquisas2022.png &")

##################################################################################
### Dados de pesquisas eleitorais em 2026
##################################################################################

library(jsonlite)

url26t1 <- paste0(
    "https://raw.githubusercontent.com/",
    "bocadojacare/agregador-eleicoes-2026/main/",
    "data/primeiro_turno/pesquisas_2026.json"
)
url26t2 <- paste0(
    "https://raw.githubusercontent.com/",
    "bocadojacare/agregador-eleicoes-2026/refs/heads/main/",
    "data/segundo_turno/pesquisas_segundo_turno.json"
)

if(!file.exists("bdj2026t1.rds")) {
    d26t1 <- as.data.frame(fromJSON(url26t1, flatten = TRUE))
    colnames(d26t1) <- gsub("candidatos.", "", colnames(d26t1), fixed = TRUE)
    d26t1$"Outros + BNI" <- 100-rowSums(d26t1[, 3:4], na.rm=TRUE)
    saveRDS(d26t1, "bdj2026t1.rds")
} else {
    d26t1 <- readRDS("bdj2026t1.rds")
}
if(!file.exists("bdj2026t2.rds")){
    d26t2 <- as.data.frame(fromJSON(url26t2, flatten = TRUE))
    colnames(d26t2) <- gsub("candidatos.", "", colnames(d26t2), fixed = TRUE)
    d26t2$BNI <- 100-rowSums(d26t2[, 3:4], na.rm=TRUE)
    saveRDS(d26t2, "bdj2026t2.rds")
} else {
    d26t2 <- readRDS("bdj2026t2.rds")
}

dat26 <- list(t1 = d26t1[, c(3,4,ncol(d26t1))],
              t2 = d26t2[,  c(3,4,ncol(d26t2))])
sapply(dat26, nrow)

## fix the data
dfn <- function(x) {
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
dat26[[1]]$Data <- dfn(d26t1$data)
dat26[[2]]$Data <- dfn(d26t2$data)

lapply(dat26, head, 2)

alldat <- c(dat22, dat26)
Dmin <- as.Date(c(Dmin22, "2026-01-21", "2026-02-01"))
Dmax <- as.Date(c(Dmax22, "2026-10-04", "2026-10-04"))
yadd <- c(20,5,10,10)

Preds26 <- lapply(1:2, function(k)
    predFn(dat26[[k]], Dmax[2+k]))

png("fourplots.png", 6000, 4000, res = 300)
par(mfcol = c(2, 2), mar = c(3,3,0,0),
    mgp = c(2,1.0,0), bty = "n", las = 1)
for(k in 1:4) {
    prdk <- c(Preds22, Preds26)[[k]]
    datFitPlotfn(alldat[[k]], prdk, 
                 ylim = c(0,55), xlim = c(Dmin[k], Dmax[k]+yadd[k]),
                 col = cores, fill = c(rgb(1:0, c(.5,1), c(0,.5), .5), gray(.5,.5)),
                 xlab = "", ylab = "%")
    abline(h = 10*(0:5), lty = 2, col = gray(0.5))
    if(k==1)
        legend("top", names(result22[[k]]), bty = "n",
               ncol = 3, lty = 1, lwd = 2, col = cores)
    legend("topleft", "", bty = "n",
           title = paste("Pesquisas", rep(c(2022, 2026), each = 2)[k],
                         "\nTurno", c(1,2,1,2)[k]))
    if(k<3) {
        legend("topright", "", title = paste("Resultado\nNom  Val"), bty = 'n')
        text(rep(Dmax22[k]+yadd[k]/2+2, 3), result22[[k]],
             format(result22[[k]], digits = 4), col = cores)
        text(rep(Dmax22[k]+yadd[k]+2, 2), res22[[k]],
             format(res22[[k]], digits = 4), col = cores[1:2])
    }
}
dev.off()

system("eog fourplots.png &")

tail(Preds26[[1]]$Lula, 1)
tail(Preds26[[1]]$Fl, 1)
