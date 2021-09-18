
#Jan Črne, izbrana leta: 2014-2016, tip terminske OM: (T=3)x(U=6)

#1. NALOGA

#uvoz podatkov
euribor2014 <- read.csv("hist_EURIBOR_2014.csv", header=TRUE, row.names = 1)
euribor2015 <- read.csv("hist_EURIBOR_2015.csv", header=TRUE, row.names = 1)
euribor2016 <- read.csv("hist_EURIBOR_2016.csv", header=TRUE, row.names = 1)

euribor14 <- euribor2014[c(1,23,43,64,84,105,126,149,170,192,215,235)]
euribor15 <- euribor2015[c(1,22,42,64,84,104,126,149,170,192,214,235)]
euribor16 <- euribor2016[c(1,21,42,63,84,106,128,149,172,194,215,237)]

e14 <- t(euribor14)
e15 <- t(euribor15)
e16 <- t(euribor16)

#skupna tabela OM
euribor <-rbind(e14,e15,e16)

#konstrukcija časovne vrste časovne vrste
casovna_vrsta_3m <- ts(euribor[,5],start = c(2014,1), end = c(2016,11), frequency = 12)
casovna_vrsta_6m <- ts(euribor[,6],start = c(2014,1), end = c(2016,11), frequency = 12)

#prikaz časovne vrste v grafu
ts.plot(casovna_vrsta_3m, casovna_vrsta_6m, gpars = list(col = c("green","red"),xlab = "Time", ylab = "%", main = "Euribor"))
legend(2016.5, 0.4, legend = c("3m","6m"), col = c("green", "red"), lty = 1:1)

#2. NALOGA

x.os <- c(12/54,2*12/54,1,2,3,6,9,12)

#graf časovne strukture OM za tri izbrane datume
plot(x.os, euribor[11,], type = "b", col = "green", xlab ="Dospetje [mesec]",
     ylab = "%", main = "Časovna struktura Euribor", 
     ylim = range(c(euribor[11,],euribor[23,], euribor[28,])))
lines(x.os, euribor[23,], type = "b", col = "blue")
lines(x.os, euribor[28,], type ="b", col = "red")
legend(9.8,-0.18, legend = c("1. 8. 2014", "3. 11. 2015", "1. 3. 2016"), col = c("green", "blue", "red"), lty = 1)

#3. NALOGA

#izračun terminskih obrestnih mer
napoved3m <- 1/ (6-3) * ((1 + euribor[,6] * 6)/ (1 + euribor[,5] * 3) - 1)
Napoved3m <- c(NA, NA, NA, napoved3m[1:33])

#tabela primerjave napovedanih OM in dejanskih OM za nek čas
primerjavaOM <- cbind(euribor[,5], euribor[,6], Napoved3m)

#izpisane točke za grafe odvisnosti
#dejanske OM
Yleto14<-euribor[,5][4:12]
Yleto15<-euribor[,5][13:24]
Yleto16<-euribor[,5][25:36]
#pricakovane OM
Xleto14 <- napoved3m[1:9]
Xleto15 <- napoved3m[10:21]
Xleto16 <- napoved3m[22:33]

#skupen graf za odvisnost napovedane in dejanske OM, na njem tudi simetrala lihih kvadrantov ter regresijska
#funkcija glede dane podatke
plot(Xleto14, Yleto14, 
     ylim=c(-0.9, 0.4), xlim=c(-0.9, 0.4),
     xlab="Napoved", ylab="Opazovano", main="3m Euribor 2014-16",
     col="red", pch = 16)
points(Xleto15,Yleto15, col="green", pch = 16)
points(Xleto16,Yleto16, col="blue", pch = 16)

legend(-0.92, 0.4, legend = c("2014", "2015", "2016"), col = c("red", "green", "blue"), pch=16)

abline(a=0,b=1,col="black",lty=2)

abline(lm(euribor[,5][4:36]~napoved3m[1:33]))


#grafi odvisnosti za vsako leto posebej
#2014
plot(Xleto14, Yleto14,
     ylim = c(0,0.4), xlim = c(0,0.4),
     xlab = "Napoved", ylab = "Opazovano", main = "3m Euribor 2014", col = "red", pch = 16)
abline(a = 0, b = 1, col = "black", lty=2)
abline(lm(Yleto14 ~ Xleto14), col="red")

#2015
plot(Xleto15, Yleto15,
     ylim = c(-0.15,0.3), xlim = c(-0.15,0.3),
     xlab = "Napoved", ylab = "Opazovano", main = "3m Euribor 2015", col ="green", pch=16)
abline(a=0, b=1, col="black", lty=2)
abline(lm(Yleto15 ~ Xleto15), col = "green")

#2016
plot(Xleto16, Yleto16, 
     ylim=c(-0.9,0.15), xlim = c(-0.9,0.15),
     xlab = "Napoved", ylab = "Opazovano", main = "3m Euribor 2016",col = "blue", pch = 16)
abline(a = 0,b = 1,col = "black", lty=2)
abline(lm(Yleto16 ~ Xleto16),col = "blue")

#e) Če bi regresijske funkcije prekrivale simetrale lihih kvadrantov, bi pomenilo, da so napovedane OM mere bile
#enake dejanskim. Glede na pozicije simetrale in regresijske funkcije opazim, da je bilo zaupanje v prihodnost trga
#najmanjše leta 2015. Če bi želeli, da hipoteza pričakovanja trga popolnoma velja, bi se regresijska in simetrala 
#marali prekrivati.







