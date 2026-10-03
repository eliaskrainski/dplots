
u0 <- 'https://www.dieese.org.br/analisecestabasica/salarioMinimo.html'
t0 <- readLines(u0)

ia <- sapply(2021:1994, function(a)
    intersect(grep(a, t0), grep('colspan', t0)))
ia

res <- Reduce('rbind', lapply(1:length(ia), function(i) {
    pa <- gregexpr('</a></td>', t0[ia[i]], fixed=TRUE)[[1]]
    a <- as.integer(substr(t0[ia[i]], pa-4, pa-1))
    i0 <- ia[i]:c(ia-1,length(t0))[i+1]
    tx <- paste(t0[i0], collapse='')
    ii <- gregexpr("R$", tx, fixed=TRUE)[[1]]
    ie <- gregexpr("</td>", tx, fixed=TRUE)[[1]]
    r <- sapply(ii, function(i)
        substr(tx, i+2, ie[which(ie>i)[1]]-1))
    if (length(r)==24)
        dn <- list(12:1, c('Nominal', 'Necessário'))
    if (length(r)==12)
        dn <- list(12:7, c('Nominal', 'Necessário'))
    else 
        dn <- list((length(r)/2):1, c('Nominal', 'Necessário'))
    data.frame(Ano=a, Mês=dn[[1]],
               matrix(as.numeric(gsub(',', '.',
                                      gsub('.', '', r, fixed=TRUE),
                                      fixed=TRUE)),
                      ncol=2, byrow=TRUE, dimnames=dn))
}))
head(res,2)
tail(res,2)
summary(res$Razão <- res$Nec/res$Nom)

brk <- c(Itamar=ISOdate(1994,12,31),
         FHC=ISOdate(2002,12,31),
         Lula=ISOdate(2010,12,31),
         Dilma=ISOdate(2016,8,31),
         Temer=ISOdate(2018,12,31),
         Bolsonaro=as.POSIXct(Sys.Date()))

dates <- ISOdate(res[,1], res[,2], 15)
ipres <- findInterval(dates, brk)+1
table(pres <- factor(ipres, labels=names(brk)))

raw <- !TRUE

res$iraz <- 1/res$Raz
rrl <- lapply(1:6, function(p) {
    ii <- which(ipres==p)
    if(raw) {
        y <- res$Raz[ii]
        r <- list(coeff=c(0, sum(y[-1])/sum(y[-length(y)]) -1))
        r$pred <- mean(res$iraz[ii])*((1+r$coef[[2]])^(mean(ii)-ii))
    } else { 
        if(p==2) {
            ii2 <- c(ii, which(ipres==1))
            aa <- lm(y ~ i, data.frame(y=res$iraz[ii2], i=max(ii2)-ii2))
            r <- list(coeff=coef(aa), pred=predict(aa)[1:length(ii)])
        } else {
            aa <- lm(y ~ i, data.frame(y=res$iraz[ii], i=max(ii)-ii))
            r <- list(coeff=coef(aa), pred=predict(aa))
        }
    }
    return(r)
})

rr <- sapply(rrl, function(x) x$coeff)
rr

xdate <- ISOdate(res[,1], res[,2], 15)
cores <- c('brown', 'blue', 'red', 'plum', 'khaki4', 'green4')

ad1 <- abs(res$iraz[1]-res$iraz)
io1 <- 120 + which.min(ad1[-(1:120)])
res[c(1, io1), ]

ad3 <- abs(res$iraz[3]-res$iraz)
io3 <- 160 + which.min(ad3[-(1:160)])
res[c(1, io3), ]

mtxt <- paste0('Atual (', format(xdate[1], '%b/%Y'),
               ') salário mínimo = ', res[1, 3],
               ' e salário necessário (DIEESE) = ', res[1, 4])

y2 <- c(10, 8, 7, 6, 5, 4)

xpres <- sapply(1:length(brk), function(x)
    pmatch(round(mean(which(ipres==x))), 1:length(dates)))
ypres <- tapply(res$iraz, ipres, max)
ypos <- ypres + c(0.01, 0, 0, 0, 0.005, 0.005)

png('salario-minimo2real.png', 1000, 600)## pdf('teste.pdf', width=10, heig=6)
par(mar=c(2,4,2,1.5), mgp=c(2.5,0.5,0), las=1)
plot(xdate, res$iraz, col=cores[ipres], axes=FALSE, 
     ylim=c(0, max(res$iraz)), type='h', lwd=2, cex.lab=2, 
     main=mtxt, xlab='', cex.main=2,
     ylab='Poder de compra do salário mínimo')
##for(p in 2:6) 
  ##  lines(xdate[ipres==p],
    ##      rrl[[p]]$p, lty=2, lwd=2)
axis(2)
axis(1, pretty(xdate, 20), format(pretty(xdate, 20), '%Y'))
abline(h=1/y2, lty=2, col=gray(0.3, 0.5), lwd=2)
axis(4, 1/y2, y2, las=1)
mtext('Número de salários mínimos necessários', 4, -1, las=3, cex=2)
text(dates[xpres], ypos - 0.003, names(brk), col=cores, cex=1.5)
##text(dates[xpres[-1]], ypos[-1] +0.007,
  ##   format(rr[2,-1]*table(ipres)[-1], digits=2),
    ## col=c('blue4', 'red4')[(rr[2,-1]<0)+1], cex=1.7)
arrows(xdate[1], res$iraz[1], xdate[io1], res$iraz[io1], lwd=3)
arrows(xdate[3], res$iraz[3], xdate[io3], res$iraz[io3], lwd=3)
text(mean(xdate[c(io1, io3)]), 0.17, 
     paste('Retrocesso à\n',
           paste(format(xdate[c(io3, io1)], '%b-%Y'), collapse='/')), cex=2.5)
legend(min(xdate), 0.23, '', bty='n', cex=1.5, 
       title=paste0('Histórico do ganho\n(ou perda)\n',
                    'do poder de compra\ndo salário mínimo'))
dev.off()
if(FALSE)
    system('eog salario-minimo2real.png &')

round(100*rr[2,], 4)
## -2.53  0.53  0.54 -0.17  0.38 -0.94
