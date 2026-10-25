setwd(here::here(""))
getwd()
source("functions.R")

###########################################################################
### Main source of pesquisas eleitorais data in em 2022:
###  https://github.com/Nexo-Dados/pesquisas-presidenciais-2022
###   + pesquisas ao 2o turno nao incluidas pela Nexo 
url22 <- paste0("https://raw.githubusercontent.com/Nexo-Dados/",
              "pesquisas-presidenciais-2022/main/")

fls22local <- fls22 <- paste0("pesquisas_", 1:2, "t.csv")
for(i in 1:2) {
    fls22local[i] <- paste0(
        "data/nexo_",
        gsub(paste0(i, "t"),
             paste0("turno", i), fls22[i]))
}
fls22used <- c(gsub("1", "1u", fls22local[1]),
               gsub("2", "2u_atualizado", fls22local[2]))
fls22used
names(fls22used) <- names(fls22local) <- names(fls22) <- c("turno1", "turno2")


instts <- c(
    "Alfa" = c("Alfa Inteligência", "Alfa/TMC"),
    "Apex/Futura" = "Apex/Futura",
    "Atlas" = c("Atlas", "Atlasinstel", "AtlasIntel"),
    "Brasmarket" = "Brasmarket",
    "BTG/Nexus" = c("BTG/Nexus", "Nexus/BTG", "Nexus/BTG Pactual"),
    "CNT/MDA" = c("CNT/MDA", "MDA"),
    "Datafolha" = c("Datafolha", "DataFolha"),
    "FSB" = "FSB",
    "Futura" = c("Futura", "Futura/Apex"),  
    "Genial/Quaest" = c("Genial/Quaest", "Quaest"),
    "Gerp" = "Gerp",
    "Idiea/BD/M" = c("Ideia", "Ideia Big Data", "Meio/Ideia"),
    "Indexa" = c("Indexa", "Indexa/Broadcast"),
    "Ipec" = c("Ipec", "Ipsos-Ipec"),
    "Ipespe" = "Ipespe",
    "Palver" = "Palver", 
    "Parana" = "Paraná Pesquisas",            
    "PoderData" = c("PoderData", "PoderData/Aya"),
    "Real Time Big Data" = "Real Time Big Data",
    "Verita" = c("Verita", "Veritá"),
    "Vox" = c("Vox", "Vox Brasil")
)

if((!file.exists(fls22used[1])) &
   (!file.exists(fls22used[1]))) {
    
    for(i in 1:2)
        if(!file.exists(fls22local[i]))
            download.file(
                url = paste0(url22, fls22[i]),
                destfile = fls22local[i])

    dat22a <- lapply(fls22local, read.csv)
    print(c(nr=sapply(dat22a, nrow)))
    
    add22t2 <- structure(list(
        Instituto = c("Atlas", "Datafolha", "IFP", "Ipec", 
                      "Quaest", "Verita", "MDA", "Paraná Pesquisas"),
        Data = c("2022-10-29", "2022-10-29", "2022-10-29", "2022-10-29",
                 "2022-10-29", "2022-10-29", "2022-10-28", "2022-10-28"),
        Data.Divulgação = c("2022-10-29T00:00:00Z", "2022-10-29T00:00:00Z",
                            "2022-10-29T00:00:00Z", "2022-10-29T00:00:00Z", 
                            "2022-10-29T00:00:00Z", "2022-10-29T00:00:00Z",
                            "2022-10-29T00:00:00Z", "2022-10-29T00:00:00Z"),
        Registro = c("BR-04838/2022", "BR-08297/2022", "sem registro localizado",
                     "BR-05256/2022", "BR-05765/2022", "BR-04217/2022",
                     "BR-01820/2022", "BR-09573/2022"),
        Lula = c(52.4, 49, 45.6, 50, 46, 45.9, 46.9, 47.1),
        Bolsonaro = c(45.7, 45, 45.3, 43, 43, 47.8, 44.9, 46.3),
        BNI = c(1.9, 6, 9.1, 7, 11, 6.3, 8.2, 6.6),
        VV13 = c(0.534, 0.52, 0.50165, 0.54, 0.52, 0.489861, 0.511, 0.504),
        VV22 = c(0.466, 0.48, 0.49835, 0.46, 0.48, 0.510139, 0.489, 0.496)),
        row.names = c(NA, 8L), class = "data.frame")

    nrow(add22t2)

    str(rbind(add22t2, dat22a$turno2))

    dat22a$turno2 <- rbind(add22t2, dat22a$turno2)
    print(c(nr=sapply(dat22a, nrow)))

    lapply(dat22a, head, 2)

    table(c(dat22a$turno1$Instituto,
            dat22a$turno2$Instituto))

    inst22 <- list(t1=rep("Outros", nrow(dat22a$turno1)),
                   t2=rep("Outros", nrow(dat22a$turno2)))
    for(k in 1:2) {
        for(j in 1:length(instts)) {
            jj <- dat22a[[k]]$Instituto %in% instts[j]
            inst22[[k]][jj] <- names(instts)[j]
        }
    }

    table(dat22a[[1]]$Instituto, inst22[[1]])
    sapply(inst22, table)
    
    dat22 <- list(
        t1 = data.frame(
            Instituto = dat22a$turno1$Instituto,
            Data = as.Date(dat22a$turno1$Data),
            dat22a$turno1[, 5:6],
            Outros = rowSums(dat22a$turno1[, 7:9], na.rm = TRUE),
            BNI = dat22a$turno1$BNI),
        t2 = data.frame(
            Instituto = dat22a$turno2$Instituto,
            Data = as.Date(dat22a$turno2$Data),
            dat22a$turno2[, 5:7])
    )

    write.table(dat22$t1, fls22used[1], row.names = FALSE, sep = ";")
    write.table(dat22$t2, fls22used[2], row.names = FALSE, sep = ";")

}

dat22 <- lapply(fls22used, read.csv, sep = ";")
for(i in 1:2)
    dat22[[i]]$Data <- as.Date(dat22[[i]]$Data)

### RESULTADOS da eleicao em 2022
ntot22 <- c(t1=123682372, t2=124252796)
bni22 <- c(t1=1964779 + 3487874, t2=1769678+3930765)
nres22 <- list(
    t1 = c(Lula      = 57259504,
           Bolsonaro = 51072345,
           "Outros" =
               sum(c(Ciro      =  3599287,
                     Tebet     =  4915423,
                     Outros    = 600955+559708+81129+53519+45620+25625+16604)),
           BNI       = bni22[[1]]),
    t2 = c(Lula      = 60345999,
           Bolsonaro = 58206354,
           BNI = bni22[[2]])
)
result22 <- list(
    "Turno 1" = 100 * (nres22$t1/ntot22[1]),
    "Turno 2" = 100 * (nres22$t2/ntot22[2]))
sapply(result22, sum)

res22 <- list( ## vv:
    "Turno 1" = 100 * nres22$t1[1:3]/(ntot22[1]-bni22[1]),
    "Turno 2" = 100 * nres22$t2[1:2]/(ntot22[2]-bni22[2]))
res22

Dmin22 <- as.Date(c("2022-01-01", "2022-08-15"))
Dmax22 <- as.Date(c("2022-10-02", "2022-10-30"))

##################################################################################
### Dados de pesquisas eleitorais em 2026
##################################################################################

library(jsonlite)

url26t1 <- paste0(
    "https://raw.githubusercontent.com/bocadojacare/",
    "agregador-eleicoes-2026/refs/heads/main/data/",
    "primeiro_turno/pesquisas_2026_normalizado.json")
url26t2 <- paste0(
    "https://raw.githubusercontent.com/bocadojacare/",
    "agregador-eleicoes-2026/refs/heads/main/data/",
    "segundo_turno/pesquisas_segundo_turno_normalizado.json")

if(!file.exists("data/bdj2026t1.rds")) {
    d26t1 <- as.data.frame(fromJSON(url26t1, flatten = TRUE))
    colnames(d26t1) <- gsub("candidatos.", "", colnames(d26t1), fixed = TRUE)
    d26t1 <- data.frame(
        d26t1[, 1:4],
        Outros = rowSums(d26t1[, 5:ncol(d26t1)], na.rm=TRUE))
    d26t1$BNI <- 100-rowSums(d26t1[, 3:5], na.rm=TRUE)
    saveRDS(d26t1, "data/bdj2026t1.rds")
} else {
    d26t1 <- readRDS("data/bdj2026t1.rds")
}
if(!file.exists("data/bdj2026t2.rds")){
    d26t2 <- as.data.frame(fromJSON(url26t2, flatten = TRUE))
    colnames(d26t2) <- gsub("candidatos.", "", colnames(d26t2), fixed = TRUE)
    d26t2$BNI <- 100-rowSums(d26t2[, 3:4], na.rm=TRUE)
    saveRDS(d26t2, "data/bdj2026t2.rds")
} else {
    d26t2 <- readRDS("data/bdj2026t2.rds")
}

if(FALSE) {
    table(c(d26t1$instituto, d26t2$instituto))
}

## fix the data
dat26 <- list(
    t1 = data.frame(Data = fixDataFn(d26t1$data),
                    d26t1[, c(3,4, -1:0+ncol(d26t1))]),
    t2 = data.frame(Data = fixDataFn(d26t2$data),
                    d26t2[, c(3,4,ncol(d26t2))])
)
sapply(dat26, nrow)

lapply(dat26, head, 2)

alldat <- c(dat22, dat26)
Dmin <- as.Date(c(Dmin22, "2026-01-21", "2026-02-01"))
Dmax <- as.Date(c(Dmax22, "2026-10-04", Sys.Date()))

ntot26 <- c(t1=125275835, NA) ## 2022 : c(123682372, 124252796)
bni26 <- c(t1=2300798 + 3674249, t2=NA) ## 2022 : c(1964779 + 3487874)
nres26 <- list(
    t1 = c(Lula      = 53879538, ##2022:57259504,
           Bolsonaro = 56104503, ##2022:51072345,
           "Outros" =
               sum(c(Cury      = 3448569, 
                     Renan     = 2675887,
                     Outros    = 2605148+326488+122911+43104+40043+22693+16881+15024)),
           BNI       = bni26[[1]]),
    t2 = c(Lula = NA,
           Bolsonaro = NA,
           BNI = NA)
)
result26 <- list(
    "Turno 1" = 100 * (nres26$t1/ntot26[[1]]),
    "Turno 2" = 100 * (nres26$t2/ntot26[[2]])#c(Lula =      100 * (0 / ntot26[2]),
#                  Bolsonaro = 100 * (0 / ntot26[2]),
 #                 BNI = 1.43+3.16)
)
sapply(result26, sum)

res26 <- list(
    "Turno 1" = 100 * nres26$t1[1:3]/(ntot26[1] - sum(bni26[[1]])),
    "Turno 2" = 100 * result26[[2]][1:2]/sum(result26[[2]][1:2]))
res26
