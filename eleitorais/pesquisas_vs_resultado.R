
setwd(here::here("eleitorais"))
getwd()

library(splines)

cbs <- function(...) {
    bb <- bs(...)
### join 1st with 2nd, and last two
    k <- ncol(bb)-2
    B <- bb[, 2:(k+1)]
    B[,1] <- bb[,1] + bb[,2]
    B[,k] <- bb[,k+1] + bb[,k+2]
    colnames(B) <- paste0("B", 1:k)
    return(B)
}
bplot <- function(B, x) {
    if(missing(x)||is.null(x))
        x <- 1:nrow(B)
    plot(x, B[,1], type = 'n')
    for(k in 1:ncol(B))
        lines(x, B[,k])
}

fls <- paste0("pesquisas_", 1:2, "t.csv")
names(fls) <- c("t1", "t2")
url <- paste0("https://raw.githubusercontent.com/Nexo-Dados/",
              "pesquisas-presidenciais-2022/main/")

for(i in 1:2)
    if(!file.exists(fls[i]))
        download.file(paste0(url, fls[i]), fls[i])

p1 <- read.csv(fls[1])
p2 <- read.csv(fls[2])
(n1 <- nrow(p1))
(n2 <- nrow(p2))

head(p1,2)
head(p2,2)

clabs <- c(colnames(p1)[c(5:10)], "OutBNI")
p1$OutBNI <- rowSums(p1[clabs[3:6]])
jj1 <- match(clabs, colnames(p1))
names(jj1) <- clabs
jj1

D1 <- as.Date(p1$Data)
D2 <- as.Date(p2$Data)
D1a <- min(D1)
D2a <- min(D2)
D1b <- max(D1)
D2b <- max(D2)

(nt1 <- as.integer(difftime(D1b, D1a-1, units = "days")))
(nt2 <- as.integer(difftime(D2b, D2a-1, units = "days")))
t1v <- 1:(nt1+1)
t2v <- 1:(nt2+3)

tail(D1a+t1v-1)
tail(D2a+t2v-1)

p1$time <- as.integer(difftime(D1, D1a-1, units = 'days'))
p2$time <- as.integer(difftime(D2, D2a-1, units = 'days'))

args(bs)
B1 <- cbs(x = t1v, knots = seq(1, nt1+1, 10), degree = 2)
B2 <- cbs(x = t2v, knots = seq(0, nt2+3, 10), degree = 2)

colSums(B1)
colSums(B2)

dim(B1)
dim(B2)

par(mfrow = c(2, 1))
bplot(B1, D1a+t1v)
bplot(B2, D2a+t2v-1)

f1 <- update(y ~ 1, paste(".~.+", paste(colnames(B1), collapse = "+")))
f2 <- update(y ~ 1, paste(".~.+", paste(colnames(B2), collapse = "+")))

head(p1)
head(p2)


fits1 <- lapply(jj1, function(j) {
    dat <- as.data.frame(B1[p1$time, ])
    dat$Date <- D1a + p1$time -1
    dat$y <- p1[,j]
    dat <- dat[complete.cases(dat),]
    r <- lm(f1, data = dat)
    r$Date <- dat$Date
    r
})

fits2 <- lapply(jj1[1:3], function(j) {
    dat <- as.data.frame(B2[p2$time, ])
    dat$Date <- D2a + p2$time -1
    dat$y <- p2[,j]
    dat <- dat[complete.cases(dat),]
    r <- lm(f2, data = dat)
    r$Date <- dat$Date
    r
})

prd1 <- lapply(fits1, function(x) {    
    pred <- predict(x, newdata=as.data.frame(B1),
                    type = 'response', se.fit = TRUE)
    p <- data.frame(Date = D1a + t1v -1,
                    fit = pred$fit)
    p$low <- pred$fit - 1.96 * pred$se.fit
    p$upp <- pred$fit + 1.96 * pred$se.fit
    return(p)
})

prd2 <- lapply(fits2, function(x) {    
    pred <- predict(x, newdata=as.data.frame(B2),
                    type = 'response', se.fit = TRUE)
    p <- data.frame(Date = D2a + t2v -1,
                    fit = pred$fit)
    p$low <- pred$fit - 1.96 * pred$se.fit
    p$upp <- pred$fit + 1.96 * pred$se.fit
    return(p)
})


cores <- c("red", "green4", "orange", "blue", "brown", "gray", "black")
cores2 <- cores[c(1,2,length(cores))]

r1 <- c(48.43, 43.2, 3.04, 4.16,
        .51+.47+.07+.05+.04+.02+.01,
        1.59+2.82)
r1[length(r1)+1] <- sum(r1[3:length(r1)])
r2 <- c(50.9, 49.1, 1.43+3.16)

plot1fn <- function() {
    plot(D1, rep(50, n1), xlim = c(D1a, D1b+5), ylim = c(0,55),
         type = "n", xlab = "Data", ylab = "%")
    abline(h = 10*(0:5), lty = 2, col = gray(0.5))
    legend("topright", "", bty = "n", title = "Resultado\n2022\nTurno 1")
    kk <- c(1,2,length(jj1)) ##1:length(jj1)
    for(k in kk) {
        points(D1, p1[, 4+k], pch = 19, cex = 2, col = cores[k])
        lines(prd1[[k]]$Date, prd1[[k]]$fit, col = cores[k], lwd = 2)
        if(k==1) {
            np <- nrow(prd1[[k]])
            polygon(prd1[[k]]$Date[c(1:np, np:1, 1)],
                    c(prd1[[k]]$low, rev(prd1[[k]]$upp), prd1[[k]]$low[1]),
                    col = rgb(1,.5,0,.5), border = 'transparent')
        }
        if(k==2) {
            np <- nrow(prd1[[k]])
            polygon(prd1[[k]]$Date[c(1:np, np:1, 1)],
                    c(prd1[[k]]$low, rev(prd1[[k]]$upp), prd1[[k]]$low[1]),
                    col = rgb(0,1,0,.5), border = 'transparent')
        }
        if(k==kk[length(kk)]) {
            np <- nrow(prd1[[k]])
            polygon(prd1[[k]]$Date[c(1:np, np:1, 1)],
                    c(prd1[[k]]$low, rev(prd1[[k]]$upp), prd1[[k]]$low[1]),
                    col = gray(0.5,.5), border = 'transparent')
        }
        segments(D1b+8, r1[k], D1b+9, r1[k], pch = "-", col = cores[k], lwd = 2)
    }
    text(rep(D1b+12, length(kk)), (r1+c(0,0,-.3,0,0,1,0))[kk],
         format(r1[kk]), col = cores[kk])
    legend("top", clabs[c(1,2,length(clabs))],
       ncol = 3, lty = 1, lwd = 2, col = cores[c(1,2,length(clabs))], bty = "n")
}
plot2fn <- function() {
    plot(D2, rep(50, n2), xlim = c(D2a, D2b+3), ylim = c(0,55),
         type = "n", xlab = "Data", ylab = "%")
    abline(h = 10*(0:5), lty = 2, col = gray(0.5))
    legend("topright", "", bty = "n", title = "Resultado\n2022\nTurno 2")
    for(k in c(1,2,3)) {
        points(D2, p2[, 4+k], pch = 19, cex = 2, col = cores2[k])
        lines(prd2[[k]]$Date, prd2[[k]]$fit, col = cores2[k], lwd = 2)
        if(k==1) {
            np <- nrow(prd2[[k]])
            polygon(prd2[[k]]$Date[c(1:np, np:1, 1)],
                    c(prd2[[k]]$low, rev(prd2[[k]]$upp), prd2[[k]]$low[1]),
                    col = rgb(1,.5,0,.5), border = 'transparent')
        }
        if(k==2) {
            np <- nrow(prd2[[k]])
            polygon(prd2[[k]]$Date[c(1:np, np:1, 1)],
                    c(prd2[[k]]$low, rev(prd2[[k]]$upp), prd2[[k]]$low[1]),
                    col = rgb(0,1,0,.5), border = 'transparent')
        }
        if(k==3) {
            np <- nrow(prd2[[k]])
            polygon(prd2[[k]]$Date[c(1:np, np:1, 1)],
                    c(prd2[[k]]$low, rev(prd2[[k]]$upp), prd2[[k]]$low[1]),
                    col = gray(0.5,.5), border = 'transparent')
        }
        segments(D2b+4, r2[k], D2b+4.4, r2[k], pch = "-", col = cores2[k], lwd = 2)
    }
    text(rep(D2b+5.2, 3), r2, format(r2), col = cores2)
}

par(mfrow = c(2, 1), mar = c(3,3,0.5,2),
    mgp = c(2,1.0,0), bty = "n", las = 1)
plot1fn()
plot2fn()

### 2026: what will happens???

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
    saveRDS(d26t1, "bdj2026t1.rds")
} else {
    d26t1 <- readRDS("bdj2026t1.rds")
}
if(!file.exists("bdj2026t2.rds")){
    d26t2 <- as.data.frame(fromJSON(url26t2, flatten = TRUE))
    saveRDS(d26t2, "bdj2026t2.rds")
} else {
    d26t2 <- readRDS("bdj2026t2.rds")
}

(n26t1 <- nrow(d26t1))
(n26t2 <- nrow(d26t2))

head(d26t1,3)
head(d26t2,3)

colnames(d26t1) <- gsub("candidatos.", "", colnames(d26t1), fixed = TRUE)
colnames(d26t2) <- gsub("candidatos.", "", colnames(d26t2), fixed = TRUE)

summary(rowSums(d26t1[, 3:ncol(d26t1)], na.rm=TRUE))

d26t1$OutBNI <- 100-rowSums(d26t1[, 3:4], na.rm=TRUE)
d26t2$OutBNI <- 100-rowSums(d26t2[, 3:4], na.rm=TRUE)

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

d26t1$Date <- dfn(d26t1$data)
d26t2$Date <- dfn(d26t2$data)

D26t1a <- min(d26t1$Date)
D26t1b <- max(d26t1$Date)
d26t1$time <- as.integer(difftime(d26t1$Date, D26t1a, units = "days"))+1

D26t2a <- min(d26t2$Date)
D26t2b <- max(d26t2$Date)
d26t2$time <- as.integer(difftime(d26t2$Date, D26t2a, units = "days"))+1

(nt26t1 <- as.integer(difftime(D26t1b, D26t1a-1, units = "days")))
t26t1v <- 1:(nt26t1+2)
tail(D26t1a+t26t1v-1)

(nt26t2 <- as.integer(difftime(D26t2b, D26t2a-1, units = "days")))
t26t2v <- 1:(nt26t2+2)
tail(D26t2a+t26t2v-1)

nt26t1
tail(seq(0, nt26t1+9, 10))
B26t1 <- cbs(x = t26t1v, knots = seq(0, nt26t1+9, 10), degree = 2)

nt26t2
tail(seq(0, nt26t2+9, 10))
B26t2 <- cbs(x = t26t2v, knots = seq(0, nt26t2+9, 10), degree = 2)

jj26t1 <- c(3,4,11)
jj26t2 <- c(3,4,5)

f26t1 <- update(y ~ 1, paste(".~.+", paste(colnames(B26t1), collapse = "+")))
f26t2 <- update(y ~ 1, paste(".~.+", paste(colnames(B26t2), collapse = "+")))

fits26t1 <- lapply(jj26t1, function(j) {
    dat <- as.data.frame(B26t1[d26t1$time, ])
    dat$Date <- D26t1a + d26t1$time -1
    dat$y <- d26t1[,j]
    dat <- dat[complete.cases(dat),]
    r <- lm(f26t1, data = dat)
    r$Date <- dat$Date
    r
})

fits26t2 <- lapply(jj26t2, function(j) {
    dat <- as.data.frame(B26t2[d26t2$time, ])
    dat$Date <- D26t2a + d26t2$time -1
    dat$y <- d26t2[,j]
    dat <- dat[complete.cases(dat),]
    r <- lm(f26t2, data = dat)
    r$Date <- dat$Date
    r
})

prd26t1 <- lapply(fits26t1, function(x) {    
    pred <- predict(x, newdata=as.data.frame(B26t1),
                    type = 'response', se.fit = TRUE)
    p <- data.frame(Date = D26t1a + t26t1v -1,
                    fit = pred$fit)
    p$low <- pred$fit - 1.96 * pred$se.fit
    p$upp <- pred$fit + 1.96 * pred$se.fit
    return(p)
})

prd26t2 <- lapply(fits26t2, function(x) {    
    pred <- predict(x, newdata=as.data.frame(B26t2),
                    type = 'response', se.fit = TRUE)
    p <- data.frame(Date = D26t2a + t26t2v -1,
                    fit = pred$fit)
    p$low <- pred$fit - 1.96 * pred$se.fit
    p$upp <- pred$fit + 1.96 * pred$se.fit
    return(p)
})

png("fourplots.png", 6000, 4000, res = 300)
par(mfcol = c(2, 2), mar = c(3,3,0,0),
    mgp = c(2,1.0,0), bty = "n", las = 1)
plot1fn()
legend("topleft", "Pesquisas 2022 - Turno 1")
plot2fn()
legend("topleft", "Pesquisas 2022 - Turno 1")
plot(d26t1$Date, rep(50, n26t1), xlim = c(D26t1a+70, D26t1b+3),
     ylim = c(0,55), type = "n", xlab = "Data", ylab = "%")
legend("topleft", "Pesquisas 2026 - Turno 1")
abline(h = 10*(0:5), lty = 2, col = gray(0.5))
for(k in c(1,2,3)) {
    j <- jj26t1[k]
    points(d26t1$Date, d26t1[, j], pch = 19, cex = 2, col = cores2[k])
    lines(prd26t1[[k]]$Date, prd26t1[[k]]$fit, col = cores2[k], lwd = 2)
    if(k==1) {
        np <- nrow(prd26t1[[k]])
        polygon(prd26t1[[k]]$Date[c(1:np, np:1, 1)],
                c(prd26t1[[k]]$low, rev(prd26t1[[k]]$upp), prd26t1[[k]]$low[1]),
                col = rgb(1,.5,0,.5), border = 'transparent')
    }
    if(k==2) {
        np <- nrow(prd26t1[[k]])
        polygon(prd26t1[[k]]$Date[c(1:np, np:1, 1)],
                c(prd26t1[[k]]$low, rev(prd26t1[[k]]$upp), prd26t1[[k]]$low[1]),
                col = rgb(0,1,0,.5), border = 'transparent')
    }
    if(k==3) {
        np <- nrow(prd26t1[[k]])
        polygon(prd26t1[[k]]$Date[c(1:np, np:1, 1)],
                c(prd26t1[[k]]$low, rev(prd26t1[[k]]$upp), prd26t1[[k]]$low[1]),
                col = gray(0.5,.5), border = 'transparent')
    }
}
legend("top", colnames(d26t1)[jj26t1], 
       ncol = 3, lty = 1, lwd = 2,
       col = cores[c(1,2,length(clabs))], bty = "n")
plot(d26t2$Date, rep(50, n26t2), xlim = c(D26t2a+70, D26t2b+3),
     ylim = c(0,55), type = "n", xlab = "Data", ylab = "%")
legend("topleft", "Pesquisas 2026 - Turno 2")
abline(h = 10*(0:5), lty = 2, col = gray(0.5))
for(k in c(1,2,3)) {
    j <- jj26t2[k]
    points(d26t2$Date, d26t2[, j], pch = 19, cex = 2, col = cores2[k])
    lines(prd26t2[[k]]$Date, prd26t2[[k]]$fit, col = cores2[k], lwd = 2)
    if(k==1) {
        np <- nrow(prd26t2[[k]])
        polygon(prd26t2[[k]]$Date[c(1:np, np:1, 1)],
                c(prd26t2[[k]]$low, rev(prd26t2[[k]]$upp), prd26t2[[k]]$low[1]),
                col = rgb(1,.5,0,.5), border = 'transparent')
    }
    if(k==2) {
        np <- nrow(prd26t2[[k]])
        polygon(prd26t2[[k]]$Date[c(1:np, np:1, 1)],
                c(prd26t2[[k]]$low, rev(prd26t2[[k]]$upp), prd26t2[[k]]$low[1]),
                col = rgb(0,1,0,.5), border = 'transparent')
    }
    if(k==3) {
        np <- nrow(prd26t2[[k]])
        polygon(prd26t2[[k]]$Date[c(1:np, np:1, 1)],
                c(prd26t2[[k]]$low, rev(prd26t2[[k]]$upp), prd26t2[[k]]$low[1]),
                col = gray(0.5,.5), border = 'transparent')
    }
}
legend("top", colnames(d26t2)[jj26t2], 
       ncol = 3, lty = 1, lwd = 2,
       col = cores[c(1,2,length(clabs))], bty = "n")
dev.off()

system("eog fourplots.png &")

tail(prd26t1[[1]], 1)
tail(prd26t1[[2]], 1)
