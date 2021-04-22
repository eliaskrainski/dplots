if(FALSE)
    setwd('..')

if(FALSE) {

    url <- paste0('http://www.ipeadata.gov.br/ExibeSerie.aspx?',
                  'stub=1&serid37796=37796&serid36482=36482')
    d0 <- readLines(url)

        library(XML)
    d1 <- readHTMLTable(d0)

    str(d1)

    tail(head(d1$grd,20),17)
    tail(d1)

    head(d1$grd_DXMainTable)
    tail(d1$grd_DXMainTable)

    igpm.d <- data.frame(
        year=substr(d1$grd_DXMainTable[-(1:4), 1], 1,4),
        month=substr(d1$grd_DXMainTable[-(1:4), 1], 6,7),
        ipcaI9312=as.numeric(gsub(',', '.',
                                  gsub('.', '', 
                                       d1$grd_DXMainTable[-(1:4), 2], fixed=TRUE),
                                  fixed=TRUE)),
        igpmI9408=as.numeric(gsub(',', '.',
                                  gsub('.', '', 
                                       d1$grd_DXMainTable[-(1:4), 3], fixed=TRUE),
                                  fixed=TRUE)))
    head(igpm.d)
    tail(igpm.d)

    write.csv(igpm.d,
              row.names=FALSE,
              file='data/ipca-igpm.csv')
    
}

dat <- read.csv('data/ipca-igpm.csv')
str(dat)

tail(dat)

dat$datex <- as.Date(paste0(dat$year, '-', sprintf('%02d', dat$month), '-15'))

dat$ipca <- c(NA, dat$ipcaI[-1]/dat$ipcaI[-nrow(dat)])-1
dat$igpm <- c(NA, dat$igpmI[-1]/dat$igpmI[-nrow(dat)])-1

source('rcode/functions.R')
ls()

dat$ipca12 <- ac12p(dat$ipca)
dat$igpm12 <- ac12p(dat$igpm)

tail(dat)

iip <- tail(1:nrow(dat), 12*25)

par(mfrow=c(2,1), mar=c(3,2,0,0), mgp=c(1.5,0.5,0))
plot(dat$datex[iip], dat$ipca[iip],
     col='blue4', lwd=2, type='l',
     bty='n', xlab='', ylab='',
     ylim=range(dat$ipca[iip], dat$igpm[iip]))
lines(dat$datex[iip], dat$igpm[iip], col='red4', lwd=2)
legend('topleft', c('IPCA', 'IGPM'),
       title='Variação mensal', col=c('blue4', 'red4'), lty=1, bty='n')
abline(h=0)
abline(h=-1:5/100, lty=2, col=gray(0.5, 0.5))
plot(dat$datex[iip], dat$ipca12[iip],
     col='blue4', lwd=2, type='l',
     bty='n', xlab='', ylab='',
     ylim=range(dat$ipca12[iip], dat$igpm12[iip]))
lines(dat$datex[iip], dat$igpm12[iip], col='red4', lwd=2)
legend('topleft', c('IPCA', 'IGPM'),
       title='Acumulado 12 meses', 
       col=c('blue4', 'red4'), lty=1, bty='n')
abline(h=0)
abline(h=-1:6/20, lty=2, col=gray(0.5, 0.5))
