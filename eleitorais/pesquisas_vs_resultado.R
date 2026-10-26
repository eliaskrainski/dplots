library(mgcv)
library(fmesher)
library(INLA)

setwd(here::here(""))
getwd()

source("datasets_prepare.R")
source("functions.R")

cores4 <- c("red", "green4", "blue4", "black")
fill4 <- c(rgb(2:0/2, c(.3,1,.5), 0:2/2, 0.5), gray(0.5,0.5))
xadd <- c(35,10,35,20)

Preds22 <- lapply(1:2, function(k)
    predFn2(dat22[[k]][, -1], Dmax22[k], h = 14))

png("pesquisas2022.png", 3000, 2000, res = 300)
par(mfrow = c(2, 1), mar = c(3,3,0,0),
    mgp = c(2,1.0,0), bty = "n", las = 1)
for(k in 1:2) {
    coresk <- cores4
    fillk <- fill4
    if(k==2) {
        coresk <- cores4[-3]
        fillk <- fill4[-3]
    }
    datFitPlotfn(dat22[[k]][,-1], prd=Preds22[[k]], 
                 ylim = c(0,60), xlim = c(Dmin22[k], Dmax22[k]+20), 
                 col = coresk, fill = fillk, 
                 xlab = "", ylab = "%")
    abline(h = 10*(0:5), lty = 2, col = gray(0.5))
    if(k==1)
        legend("top", names(result22[[k]]), bty = "n",
               ncol = 4, lty = 1, lwd = 2, col = coresk)
    legend("topleft", "", bty = "n",
           title = paste("Pesquisas 2022 -", names(result22)[k]))
    legend("topright", "", title = paste("Resultado\nNom  Val"), bty = 'n')
    text(rep(Dmax22[k]+10, 3), result22[[k]],
         format(result22[[k]], digits = 4), col = coresk)
    text(rep(Dmax22[k]+20, 2), res22[[k]],
         format(res22[[k]], digits = 4), col = coresk)
}
dev.off()

system("eog pesquisas2022.png &")

##################################################################################
### Dados de pesquisas eleitorais em 2026
##################################################################################
##library(splines)
dat26 <- lapply(dat26, function(d) d[d$BNI>0,])
alldat <- c(lapply(dat22, function(d) d[,-1]),dat26)
Preds26 <- lapply(1:2, function(k) {
    predFn2(dat26[[k]], Dmax[2+k], h = 21)
})

fl4 <- paste0("fourplots_at", Sys.Date(), ".png")
png(fl4, 6000, 4000, res = 300)

par(mfcol = c(2, 2), mar = c(3,3,0,0),
    mgp = c(2,1.0,0), bty = "n", las = 1)
for(k in c(1:4)) {
    coresk <- cores4
    fillk <- fill4
    if(k%in%c(2,4)) {
        coresk <- cores4[-3]
        fillk <- fill4[-3]
    }
    prdk <- c(Preds22, Preds26)[[k]]
    datFitPlotfn(alldat[[k]], prd = prdk, 
                 ylim = c(0,55), xlim = c(Dmin[k], Dmax[k]+xadd[k]),
                 col = coresk, fill = fillk,
                 xlab = "", ylab = "%")
    abline(h = 10*(0:5), lty = 2, col = gray(0.5))
    if(k==1)
        legend("top", names(result22[[k]]), bty = "n",
               ncol = 4, lty = 1, lwd = 2, col = coresk)
    legend("topleft", "", bty = "n",
           title = paste("Ano", rep(c(2022, 2026), each = 2)[k],
                         "\nTurno", c(1,2,1,2)[k],
                         ":", nrow(alldat[[k]]), " pesquisas"))
    pk <-  sapply(prdk, function(p) tail(p$fit,1))
    text(rep(Dmax[k]+c(7,2,5,5)[k], length(prdk)), pk,
         format(pk, digits = 3), col = coresk)
    if(k<3) {
        legend("topright", "", title = paste("Resultado\nNominal  Valido"), bty = 'n')
        text(rep(Dmax[k]+xadd[k]-6/k, 3), result22[[k]],
             format(result22[[k]], digits = 3), col = coresk)
        text(rep(Dmax[k]+xadd[k]+4/k, 2), res22[[k]],
             format(res22[[k]], digits = 3), col = coresk)
    }
    if(k>=3) {
        legend("topright", "", title = paste("Resultado\nNom  Val"), bty = 'n')
        text(rep(Dmax[k]+xadd[k]-4, 3), result26[[k-2]],
             format(result26[[k-2]], digits = 3), col = coresk)
        text(rep(Dmax[k]+xadd[k]+4, 2), res26[[k-2]],
             format(res26[[k-2]], digits = 3), col = coresk)
    }
    abline(v = Dmax[k]+c(12.5,3.5,8,8)[k], col = 4, lty = 2)
}

dev.off()

system(paste("eog", fl4, "&"))

tail(Preds26[[1]]$Lula, 1)
tail(Preds26[[1]]$Fl, 1)

tail(d26t1[order(dat26[[1]]$Data), c(1,2,3,4)])

## 2022 and 2026
rcfn <- function(k) {
    d <- data.frame(
        pools = sapply(c(Preds22, Preds26)[[k]],
                           function(x) tail(x$fit, 1)))
    d$Resultado = c(result22, result26)[[k]]
    d$diff = d$Resultado - d$p
    d$relDiff = d$diff/d$Resultado 
    d
}

rc <- list(
    ano2022 = list(Turno1=rcfn(1), Turno2=rcfn(2)),
    ano2026 = list(Turno1=rcfn(3), Turno2=rcfn(4)))

lapply(rc, function(x) round(x$Turno1, digits = 2))
lapply(rc, function(x) round(x$Turno2, digits = 2))

