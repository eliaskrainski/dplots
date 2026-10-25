
allpools <- read.csv("https://www.placardaspesquisas.com.br/data/polls.csv")

table(allpools$race)

table(allpools$scenario[(allpools$race=="presidente") &
                    (allpools$round == 2)])

p2pools <- allpools[(allpools$race=="presidente") &
                    (allpools$scenario == "2º turno: Lula e Flávio Bolsonaro"), ]
dim(p2pools)

table(p2pools$candidate)

tail(p2pools[c("pollster", "fieldwork_end", "sample_size", "candidate", "pct")])
table(p2pools$pollster)

p2pools$pDp <- paste(p2pools$fieldwork_end, p2pools$pollster)
length(unique(p2pools$pDp))

dataf <- do.call("rbind", lapply(sort(unique(p2pools$pDp)), function(p) {
    ii <- p2pools$pDp==p
    il <- ii & (p2pools$candidate=="Lula")
    ib <- ii & (p2pools$candidate=="Flávio Bolsonaro")
    stopifnot(sum(il)==1)
    stopifnot(sum(ib)==1)
    d <- data.frame(
        Instituto = p2pools$pollster[il],
        Data = p2pools$fieldwork_end[il],
        n = p2pools$sample_size[il],
        Lula = round((p2pools$pct[il] / 100) * p2pools$sample_size[il]),
        Flavio = round((p2pools$pct[ib] / 100) * p2pools$sample_size[ib]))
    d$BNI <- d$n - d$Lula - d$Flavio
    d
})); dataf$Data <- as.Date(dataf$Data)

head(dataf)
tail(dataf)

sort(table(dataf$Instituto))

dataf[dataf$BNI==0, ]

par(mar = c(3,3,0.5,0.5), mgp = c(2,1,0), bty = "n")
with(dataf, plot(Data, Lula/n, pch = 8, col = 2, ylim = c(0,.6)))
with(dataf, points(Data, Flavio/n, pch = 3, col = "green4"))
with(dataf, points(Data, 1-(Lula + Flavio)/n, pch = 3, col = "gray"))
abline(h=0, lty = 2, col = gray(0.5, 0.5))

summary(dataf$Data)
D0 <- min(dataf$Data)-1
dataf$Dia <- as.numeric(difftime(
    dataf$Data, D0, units = "days"))

head(dataf,2)
tail(dataf,2)

library(fmesher)
library(INLA)

nt <- max(dataf$Dia)
t0 <- 1:nt
length(tk <- rev(seq(max(dataf$Dia), -10, -21)))

tmesh <- fm_mesh_1d(tk, degree = 2)
tmesh$loc

Z <- fm_evaluator(mesh = tmesh, loc = c(t0, dataf$Dia))$proj$A
dim(Z)

dfit <- data.frame(
    Dia = c(t0, dataf$Dia),
    Validos = c(rep(1, length(t0)), dataf$Lula + dataf$Flavio),
    Lula = c(rep(NA, length(t0)), dataf$Lula),
    t = NA
)

ff <- Lula ~ 0 +
    f(t, A.local = Z, constr = FALSE,
      model = "rw2", values = 1:tmesh$n,
      scale.model = TRUE,
      hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.1))))

ifit <- inla(
    formula = ff,
    family = "binomial",
    control.predictor = list(link=1),
    control.compute = list(return.marginals.predictor = TRUE),
    Ntrials = Validos,
    data = dfit
)

dim(prd <- ifit$summary.fitted.values[1:length(t0),])
head(prd,3)

png("pool_placar.png", 3600, 2400, res = 200)

par(mfrow = c(1, 2), mar = c(3,3,0.5,0.5), mgp = c(2,1,0), bty = "n")
with(dataf, plot(Data, Lula/(Lula+Flavio), pch = 8, col = 2))
polygon(D0+c(t0, rev(t0),1),
        c(prd[, 3], rev(prd[,5]), prd[1,3]),
        col = gray(0.5,0.5), border = 'transparent')
lines(D0+t0, prd$mean, lty = 2)
points(as.Date("2026-10-04"), 53879538/(53879538 + 56104503),
       col = 'green4', pch = 19)
legend("top", paste(nrow(dataf), "pesquisas consideradas"),
       title = "Scenario turno 2: Lula vs Flavio", bty = "n")
legend("topright", bty = "n",
       c("Pesquisas", "Res. turno 1",
         "pool : fitted", "pool : fitted 95% CI"),
       pch = c(8, 19,0,0), col = c(2, "green4", 1,0), fill = c(0,0,0,"gray"),
       border = c(0,0,0,0), pt.cex = c(1,1,0,0), lty = c(0,0,2,0))
legend("bottomright",
       paste0("https://www.placardaspesquisas.com.br/data/polls.csv"),
       title = "Fonte dos dados:", bty = "n")
abline(h = 0.5, lty = 2)
plot(inla.smarginal(ifit$marginals.fitted.values[[nt]]), type = "l",
     xlab = "P(Lula)", ylab = "Density")
legend("top", "", bty = "n", title = "Probabilidade (a posteriori)")
abline(h = 0, v = 0.5, lty = 2)

dev.off()

tail(prd[,c(1,2,3,5)],1)
inla.pmarginal(0.5, ifit$marginals.fitted.values[[nt]])


system("eog pool_placar.png &")
