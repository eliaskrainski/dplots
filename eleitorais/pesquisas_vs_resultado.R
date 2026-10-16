library(mgcv)

setwd(here::here("eleitorais"))

source("datasets_prepare.R")
source("functions.R")

cores4 <- c("red", "green4", "blue4", "black")
fill4 <- c(rgb(2:0/2, c(.3,1,.5), 0:2/2, 0.5), gray(0.5,0.5))
xadd <- c(35,10,35,10)

Preds22 <- lapply(1:2, function(k)
    predFn(dat22[[k]], Dmax22[k]))##, h = 30))

png("pesquisas2022.png", 3000, 2000, res = 300)
par(mfrow = c(2, 1), mar = c(3,3,0,0),
    mgp = c(2,1.0,0), bty = "n", las = 1)
for(k in 1:2) {
    coresk <- cores4
    fillk <- fill4
    if(k==2) {if(!file.exists("data/bdj2026t2.rds")){
    d26t2 <- as.data.frame(fromJSON(url26t2, flatten = TRUE))
    colnames(d26t2) <- gsub("candidatos.", "", colnames(d26t2), fixed = TRUE)
    d26t2$BNI <- 100-rowSums(d26t2[, 3:4], na.rm=TRUE)
    saveRDS(d26t2, "data/bdj2026t2.rds")
} else {
    d26t2 <- readRDS("data/bdj2026t2.rds")
}

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

dat26 <- list(
    t1 = data.frame(Data = dfn(d26t1$data),
                    d26t1[, c(3,4, -1:0+ncol(d26t1))]),
    t2 = data.frame(Data = dfn(d26t2$data),
                    d26t2[, c(3,4,ncol(d26t2))])
)
sapply(dat26, nrow)

lapply(dat26, head, 2)

alldat <- c(dat22, dat26)
Dmin <- as.Date(c(Dmin22, "2026-01-21", "2026-02-01"))
Dmax <- as.Date(c(Dmax22, "2026-10-04", "2026-10-16"))

        coresk <- cores4[-3]
        fillk <- fill4[-3]
    }
    datFitPlotfn(dat22[[k]], prd=Preds22[[k]], 
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
Preds26 <- lapply(1:2, function(k)
    predFn(dat26[[k]], Dmax[2+k]))

png("fourplots.png", 6000, 4000, res = 300)
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
         format(pk, digits = 2), col = coresk)
    if(k<3) {
        legend("topright", "", title = paste("Resultado\nNominal  Valido"), bty = 'n')
        text(rep(Dmax[k]+xadd[k]-6/k, 3), result22[[k]],
             format(result22[[k]], digits = 3), col = coresk)
        text(rep(Dmax[k]+xadd[k]+4/k, 2), res22[[k]],
             format(res22[[k]], digits = 3), col = coresk)
    }
    if(k==3) {
        legend("topright", "", title = paste("Resultado\nNom  Val"), bty = 'n')
        text(rep(Dmax[k]+xadd[k]-4, 3), result26[[k-2]],
             format(result26[[k-2]], digits = 3), col = coresk)
        text(rep(Dmax[k]+xadd[k]+4, 2), res26[[k-2]],
             format(res26[[k-2]], digits = 3), col = coresk)
    }
    abline(v = Dmax[k]+c(12.5,3.5,8,8)[k], col = 4, lty = 2)
}
dev.off()

system("eog fourplots.png &")

tail(Preds26[[1]]$Lula, 1)
tail(Preds26[[1]]$Fl, 1)

tail(d26t1[order(dat26[[1]]$Data), c(1,2,3,4)])

## 2022 and 2026
rcfn <- function(k) {
    d <- data.frame(
        poolPesquisas = sapply(c(Preds22, Preds26)[[k]],
                           function(x) tail(x$fit, 1)))
    d$Resultado = c(result22, result26)[[k]]
    d$diff = d$Resultado - d$p
    d$relDiff = d$diff/d$Resultado-1
    d
}

rc <- list(
    ano2022 = list(Turno1=rcfn(1), Turno2=rcfn(2)),
    ano2026 = list(Turno1=rcfn(3), Turno2=rcfn(4)))

lapply(rc, lapply, round, 2)
