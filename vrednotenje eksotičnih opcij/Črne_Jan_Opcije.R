library(combinat)
library(Rlab)

#uvoz tabele cen delnic skozi obdobja
p1 <- c(50.00, 52.50, 49.88, 52.37, 49.75, 52.24)
p2 <- c(50.00, 52.50, 55.12, 57.88, 60.78, 63.81)
p3 <- c(50.00, 47.50, 49.88, 47.38, 45.01, 42.76)
p4 <- c(50.00, 47.50, 45.12, 47.38, 49.75, 52.24)
p5 <- c(50.00, 52.50, 49.88, 52.37, 54.99, 57.74)

#funkcija, ki vrne izplačilo opcije ob zapadlosti, to le to določi glede na razvoj cene delnice 
izplacilo <- function(vrsta, T, type = c("put", "call")) {
  if (type == "put")
    return(max(-min(vrsta[1:T]) +  min(vrsta[(T+1):length(vrsta)]), 0))   
  if (type == "call")
    return(max(-max(vrsta[1:T]) +  max(vrsta[(T+1):length(vrsta)]), 0))   
}

#funkcija, ki vrne premije ob vseh možnih binomskih razvojih trga
binomski <- function(S0, u, d, U, R, T, type){
  q = (1 + R - d)/(u - d)  #verjetnost
  hipk <- hcube(rep(2, U), translation = -1) #hiperkocka
  Q <- q^rowSums(hipk) * (1-q)^(U - rowSums(hipk))   #matrika verjetnosti s katerimi pomnožimo cene,
                                                     #koeficienti izračunane glede na število dobrih oz. slabih
                                                     #razvojev trga, glede na to koliko v v hiperkocki enk, toliko je
                                                     #dobrih razvojev
  matrika <- cbind(rep(S0, 2^U), u^(hipk) * d^(1-hipk))   #matrika koeficientov s katerimi pomnožimo cene v obdobjih glede na št. dobrih
                                                          #razvojev
                                                          #cbind doda še stolpec z začetno ceno
  prod <- t(apply(matrika, 1, cumprod)) #matrika z cenami delnice v različnih obdobjih
  izplacila <- apply(prod, 1, izplacilo, T = T, type=type)  #vektor izplačil, če za izračun uporabimo ekstrmni
                                                            #razkorak
  premija <- sum(Q * izplacila) / (1 + R)^U #izračun premije glede na izplačila ob zapadlosti in verjetnosti da smo v
                                        #določenem stanju
  return(premija)
}  
# #test; rezultati morajo biti enaki oz zelo podobni (napake zaradi zaokroževanja)
# print(paste(c(binomski(50,1.05,0.95,5,0.03,3,"call"), 4.463501)))
# print(paste(c(binomski(50,1.05,0.95,5,0.03,3,"put"), 4.108056)))
# print(paste(c(binomski(50, 1.05, 0.9 , 10, 0.03, 5, "call"), 8.626511)))
# print(paste(c(binomski(60, 1.05, 0.95, 15, 0.01, 8, "put"), 4.977736)))
# print(paste(c(binomski(70, 1.05, 1   ,  7,    0, 5, "call"), 0)))
# print(paste(c(binomski(80, 1.1 , 0.95,  9, 0.05, 4, "put"), 10.89925)))
# print(paste(c(binomski(90, 1.15, 0.8 , 10, 0.01, 3, "call"), 27.91311)))

#N je št. simuliranih poti
monte <- function(S0, u, d, U, R, T, type, N){
  q = (1 + R - d) / (u - d)
  for (i in 1:N){    #naredimo N simulacij poti
    if (i == 1){
      mat <- rbinom(U, 1, q)   #glede na binomsko porazdelitev zgenerira naključno vrstico poljubno velike hiperkocke
                               #pri i=1 ustvari prvo vrstico matrike
    }
    else{
      mat <- rbind(mat, rbinom(U, 1, q))    #ustvarimo naključno matriko, podobna obliki hiperkocke, le drugačen
                                            #vrstni red vrstic
    }
  } 
  matrika <- cbind(rep(S0, 2^U), u^(mat) * d^(1-mat))   
  prod <- t(apply(matrika, 1, cumprod))
  izplacila <- apply(prod, 1, izplacilo, T=T, type=type)  
  premija <- sum(izplacila) / (N * (1 + R)^U) 
  return(premija)
}

# S0=60
# u=1.05
# d=0.95
# U=15
# R=0.01
# T=8
# N1=10
# N2=100
# N3=1000
# mon1 <- monte(S0,u,d,U,R,T,'put',N1)
# mon2 <- monte(S0,u,d,U,R,T,'put',N2)
# mon3 <- monte(S0,u,d,U,R,T,'put',N3)

#TESTIRANJE, odstopnja so normalna, pri večih ponovitvah bi morala biti manjša oz. manj pogosto večja
#print(paste(c(monte(50,1.05,0.9,10,0.03,5,"call",100), 8.832116)))
#print(paste(c(monte(70, 1.05, 1, 7, 0, 5, "put", 2000), 0)))
#print(paste(c(monte(90, 1.15, 0.8 , 10, 0.01, 3, "call",50000),28.0376)))


#M je število ponovitev Monte Carlo simulacije
MonteCarlo <- function(S0, u, d, U, R, T, type, N, M){
  sim_premije = numeric(M)
  #sim_premije = 1:M
  for (i in 1:M){
    #k=monte(S0,u,d,U,R,T,type,N)
    sim_premije[i] = monte(S0,u,d,U,R,T,type,N)
  }
  return(sim_premije)
}

#TEST2, 3 ponovitve Monte Carlo metode za 50 000 zgeneriranih vzrorcev vrnejo zelo podobne rezultate, zgleda pravilno
#MonteCarlo(90, 1.15, 0.8, 10, 0.01, 3, "call", 50000, 3)


#GRAFI
S0 = 60
u = 1.05
d = 0.95
U = 15
R = 0.01
T = 8
N1 = 10
N2 = 100
N3 = 1000

#1)
MCM <- MonteCarlo(S0,u, d, U, R, T, "put", N1, 100)
povprečje_MCM <- mean(MCM)  #povprečna vrednost
st_odk_MCM <- sd(MCM)  #standardni odklon

hist(MCM, breaks = 20, xlim = c(0, 10),
     main = "Monte Carlo : N = 10", xlab = "Premija", ylab = "Frekvenca")
abline(v = povprečje_MCM,
       col = "green", lwd = 3)   #navpičnica, ki prikazuje povprečno vrednost Monte Carlo metode
abline(v = binomski(S0, u, d, U, R, T, "put"),
       col = "red", lwd = 3, lty = 3)   #navpičnica, ki prikazuje teoretično oceno premije, brez generiranja binomsko
                                        #porazdeljenih ravojev trga

#puščici, ki prikazujeta standardni odklon
arrows(x0 = povprečje_MCM, y0 = 0, x1 = povprečje_MCM + st_odk_MCM, y1 = 0, 
       col = "green", lwd = 2)
arrows(povprečje_MCM, 0, povprečje_MCM - st_odk_MCM, 0,
       col = "green", lwd = 2)

legend("topright", c("Monte Carlo","analiza modela"), col = c("green", "red"),
       lwd = c(2, 2), lty = c(1, 3))

#2)enako kot 1) le večji vzorec
MCM2 <- MonteCarlo(S0, u, d, U, R, T, 'put', N2, 100)
povprečje_MCM2 <- mean(MCM2)
st_odk_MCM2 <- sd(MCM2)

hist(MCM2, breaks=20, xlim = c(0, 10),
     main = "Monte Carlo : N = 100", xlab = "Premija", ylab = "Frekvenca")

abline(v = povprečje_MCM2, col = "green", lwd = 3)
abline(v = binomski(S0, u, d, U, R, T, "put") , col = "red", lty = 3, lwd = 3)

arrows(povprečje_MCM2, 0, povprečje_MCM2 + st_odk_MCM2, 0, col = "green", lwd=2)
arrows(povprečje_MCM2, 0, povprečje_MCM2 - st_odk_MCM2, 0, col = "green", lwd=2)

legend("topright", c("Monte Carlo", "analiza modela"),col = c("green", "red"),
       lwd = c(2,2), lty = c(1,3))

#3)enako kot 1) le večji vzorec
MCM3 <- MonteCarlo(S0, u, d, U, R, T, "put", N3, 100)
povprečje_MCM3 <- mean(MCM3)
st_odk_MCM3 <- sd(MCM3)

hist(MCM3, breaks = 20, xlim = c(0, 10), 
     main = "Monte Carlo : N = 1000", xlab = "Premija", ylab = "Frekvenca")

abline(v = povprečje_MCM3, col = "green", lwd = 3)
abline(v = binomski(S0, u, d, U, R, T, "put"), col = "red", lty = 3, lwd = 3)

arrows(povprečje_MCM3, 0, povprečje_MCM3 + st_odk_MCM3, 0, col = "green", lwd=2)
arrows(povprečje_MCM3, 0, povprečje_MCM3 - st_odk_MCM3, 0, col = "green", lwd=2)

legend("topright", c("Monte Carlo", "analiza modela"), col = c("green","red"),
       lwd=c(2, 2),lty=c(1, 3))