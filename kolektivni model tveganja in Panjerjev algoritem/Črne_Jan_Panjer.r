library("actuar")

vzorec4 <- scan("vzorec4.txt")

histogram <- hist(vzorec4, 19, main = "Histogram odškodnin",xlab = "Višina odškodnine", ylab = "Frekvenca")

#izračun parametrov za paretovo porazdelitev, glede na dan vzorec
parametri <- mde(vzorec4, ppareto1, start = list(shape = 1, min = 1), measure = "CvM")
#ispiše parametre v vektor
parametri1 = parametri[[1]]


#še en histogram, ki sedaj na y-osi kaže gostoto, ne več frekvence
hist(vzorec4, ylim = c(0, 2), probability = TRUE,
     main = "Histogram odškodnin", xlab = "Višina odškodnine", ylab = "Gostota")
#na graf izriše gostoto, glede na naše naračunane parametre
curve(dpareto1(x, min = parametri1["min"], shape = parametri1["shape"]), col = "red",add = TRUE) 
legend("topright", "Paretova porazdelitev",col="red", lwd = 2)    

#empirična porazdelitvena funkcija
plot(ecdf(vzorec4), 
     main = "Porazdelitvena funkcija odškodnin", xlab = "Višina odškodnine",ylab = "Porazdelitvena funkcija")
#porazdelitvena funkcija glede na naše parametre
curve(ppareto1(x, min = parametri1["min"], shape = parametri1["shape"]), add = TRUE, col = "red")
legend("right", c("Empirična porazdelitev", "Paretova porazdelitev"),
       col = c("black", "red"), pch = c(16, NA), lwd = c(1,2))    

#prič. vrednost škodnih zahtevkov
EY <- mean(vzorec4)
#pričakovana vrednost naključno generiranega vzorca iz binomske porazdelitve
EN <- mean(rbinom(25, 25, 0.5))
#pričakovana vrednost komulativne škode, glede na generiran vzorec
ES <-  EY * EN
#varianca komulativne škode, glede na generiran vzorec
VarS = EN * var(vzorec4) + ((EY)^2) * var(rbinom(25, 25, 0.5))

#zvezne funkcije podiskreti, vsota tega vektorja je skoraj 1
diskretizacija <- discretize(ppareto1(x, min = parametri1["min"], shape = parametri1["shape"]),
                             0, 8, step = 0.1, method = "rounding")   
diskretizacija1 <- c(0, diskretizacija)

#narišemo funkcijo, ki izriše fino diskretno porazdelitev
plot(stepfun(seq(0, 7.9, 0.1), cumsum(diskretizacija1)),
     main = "Paretova porazdelitev", xlab = "x", ylab = "Porazdelitvena funkcija", col = "blue", do.points = FALSE)
#primerjamo z zvezno porazdelitveno funkcijo
curve(ppareto1(x, min = parametri1["min"], shape = parametri1["shape"]), add = TRUE, col = "red")

#izračun porazdelitve komulativen škode s panjerjevim algoritmom (recursive),
porazdelitevS <- aggregateDist(method = "recursive", model.freq = "binomial",
                               p0 = NULL, model.sev = diskretizacija1, x.scale = 0.1,
                               size = 25, prob = 0.5, maxit = 1000)   


#tokrat E(S) izračunamo iz porazdelitvene funkcije S glede na naš vzorec in N, dobimo manj "teoretičen" rezultat
ES2 <- mean(porazdelitevS)
#enako za Var(S)
VarS2 <- sum(diff(porazdelitevS) * knots(porazdelitevS)^2) - ES^2


#generiramo binomsko porazdeljen vzorec z velikostjo 10000 in vrednistmi med 1 in 25
simulacijaN <- rbinom(10000, 25, 0.5)
#ustvari prazen list oz. vektor, ki ga nato napolni z rezultati Monte Carlo simulacije
simulacijaS <- NA
#tisočkrat ponovimo izračun komulativnih škod
for (i in 1:10000){
  simulacijaS[i] <- sum(rpareto1(simulacijaN[i], parametri1["shape"], parametri1["min"]))
  }


#povprečna vrednost S, tokrat naračunana s pomočjo Monte Carlo simulacije
ES3 <- mean(simulacijaS)
#enako za varianco
VarS3 <- var(simulacijaS)

#izris porazdelitve S s panjerjem
plot(porazdelitevS, col = "red")
#izris porazdelitve S z Monte Carlo
plot(ecdf(simulacijaS), col = "blue",add = TRUE)
legend(70, 0.3, legend = c("Panjerjev algoritem", "Monte Carlo simulacija"),
       col=c("red","blue"), lwd = 1)
