library("dplyr")
platina <- read.csv("platina20.csv")
plat <- platina[1:131,]  #vzamemo prvih 131 vrstic, kar predstavlja podatke za zadnjih 6 mesecev

plat[2:5] <- lapply(plat[2:5], function(x) gsub("\\$", "", x)) #iz datafram-a odstranimo znake za dolar
plat[2:5] <- lapply(plat[2:5], function(x) gsub(",", "", x))  #odstranimo vejico, ki dela probleme pri tisočicah
p_zakl <- plat[5]

ts_p <- ts(p_zakl, start = 1, end = nrow(p_zakl), frequency = 1)  #ts_p[114] tako klicemo posamezne vr. iz ts-jev
ts.plot(ts_p,
        gpars = list(xlab = "Time", ylab = "USD", main = "Platina"))
points(ts_p, pch=16)

G <- function(vrsta, k){
  y = NULL
  for (i in k+1 : length(vrsta)){
    y[i]= sum(vrsta[(i-1) : (i-k)]) / k   #enačba za glajene vrednosti
  }
  return(y[(k+1) : (length(vrsta)+1)])
}


ts.plot(ts_p,
        gpars = list(xlab="Time", ylab="USD", main="Drseče povprečje"),
        xlim=c(0,140))
points(ts_p,pch=16)
lines((7:131), G(ts_p, 7),
      col="red", lw = 3)
napoved <- G(ts_p,7)[length(G(ts_p, 7))] #vzame zadnnjo vrednost return-a funkcije G, to je naša napoved za prihodnost
lines((131:140), rep(napoved, 10),
      col = "red",lw = 3)


#izračun srednje kvadratne napake
MSE <- function(vrsta,k){
  skn = sum((vrsta[k:131]-G(vrsta,k))^2) / (length(vrsta)-k)
  return(skn)
}

MSE7 <- MSE(ts_p, 7)


par(mfrow = c(2, 2))  #naslednjih axb grafov bo razporejenih na isto stran, v nasem primeru v 2x2 obliki
#glajenje reda 7
ts.plot(ts_p,
        gpars=list(xlab = "Time", ylab = "USD", main = "Drseče povprečje reda 7"),
        xlim = c(0, 140))
points(ts_p, pch = 16)
lines((7:131),G(ts_p, 7),col="red",lw=2)  #prvih 7 vrednosti formula za glajenje ne more naracunati
napoved <- G(ts_p,7)[length(G(ts_p, 7))]
lines((131:140), rep(napoved, 10),
      col = "red", lw = 3)

#glajeneje reda 14
ts.plot(ts_p,
        gpars=list(xlab = "Time", ylab = "USD", main = "Drseče povprečje reda 14")
        ,xlim = c(0, 140))
points(ts_p, pch = 16)
lines((14:131), G(ts_p, 14),
      col="red", lw = 3)
napoved <- G(ts_p, 14)[length(G(ts_p, 7))]
lines((131:140), rep(napoved, 10),
      col = "red",lw = 3)

MSE14 <- MSE(ts_p, 14)

#glajenje reda 30
ts.plot(ts_p,
        gpars = list(xlab="Time", ylab="USD", main="Drseče povprečje reda 30")
        ,xlim=c(0,140))
points(ts_p, pch=16)
lines((30:131), G(ts_p, 30),col="red",lw = 3)
napoved <- G(ts_p, 14)[length(G(ts_p, 30))]
lines((131:140), rep(napoved, 10),
      col = "red",lw = 3)

MSE30 <- MSE(ts_p, 30)

#eksponentno glajenje
EG <- function(vrsta, alpha){
  l = NULL
  l[1] = vrsta[1]  #ustvari zacetno pogoj za rekurzijo
  for (i in 2 : (length(vrsta))){
    l[i] = alpha * vrsta[i] + (1-alpha) * l[i-1]  #rekurzivna enačba za eksponentno glajenje
  }
  return(l)
}

#definicija in graf casovne vrste za eksponentno glajenje
par(mfrow = c(1, 1)) #ustavi izris grafov na isto stran 
#eksponentno glajenje je za alfa=0.15, alfa je element (0.1, 0.3)
ts.plot(ts_p,
        gpars = list(xlab="Time", ylab = "USD", main = "Eksponentno glajenje")
        ,xlim=c(0,140))
points(ts_p, pch = 16)
lines((1:131), EG(ts_p, 0.15),
      col = "red", lw = 3)
napoved <- EG(ts_p, 0.15)[length(EG(ts_p, 0.15))]   
lines((131:140), rep(napoved, 10),
      col = "red", lw = 3)



