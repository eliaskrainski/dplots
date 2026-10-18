library(mgcv)

setwd(here::here(""))
getwd()

cores4 <- c("red", "green4", "blue4", "black")
fill4 <- c(rgb(2:0/2, c(.3,1,.5), 0:2/2, 0.5), gray(0.5,0.5))
xadd <- c(35,10,35,10)

p1 <- plogis(-.5+0.2*sin(2*pi*(1:100/100)))
plot(p1)

a <- data.frame(i = 1:100, y1 = rbinom(100, 500, p1))
a$y2 <- rbinom(100, 5000, p1)
a$n1 <- 500 - a$y1
a$n2 <- 5000 - a$y2
head(a,2)

b1 <- gam(cbind(y1, n1)~s(i),family=binomial,data=a)
b2 <- gam(cbind(y2, n2)~s(i),family=binomial,data=a)
d1 <- gam(p ~ s(i), data=data.frame(p=a$y1/500,i=a$i))
d2 <- gam(p ~ s(i), data=data.frame(p=a$y2/5000,i=a$i))

pFn <- function(fitt, newd, D0) {
    prd <- predict.gam(fitt, newdata = newd,
                       type = "response", se.fit = TRUE)
    prd <- data.frame(Date = D0, fit = prd$fit, se.fit = prd$se.fit)
    prd$low <- prd$fit - 1.96 * prd$se.fit
    prd$upp <- prd$fit + 1.96 * prd$se.fit
    return(prd)
}

Dmax <- Sys.Date()
Dmin <- Dmax-100 

pl <- lapply(list(b1,b2,d1,d2), pFn, newd=data.frame(i=1:100), D0=Dmin+1:100)

par(mfcol = c(2,2), mar = c(3,3,1.5,0.5), mgp = c(2,0.5,0), bty = "n")
for(k in c(1:4)) {
    plot(Dmin+1:100, cbind(a$y1/500, a$y2/5000, a$y1/500, a$y2/5000)[,k],
         pch = 19, xlab = "", ylab = "p")
    polygon(Dmin+c(1:100, 100:1, 1),
            c(pl[[k]]$low, rev(pl[[k]]$upp), pl[[k]]$low[1]),
            col = fill4[k], border = 'transparent')
    lines(Dmin+1:100, pl[[k]]$fit,
          col = cores4[1], lty = 1, lwd = 2)
    lines(Dmin+1:100, p1, lty = 2, lwd = 2)
}
