#zadanie 2

install.packages("tidyverse")
choroba <- read.csv2(file="Choroba.csv",header=TRUE)


# ZADANIE 1
library(binom)

procent_pokrycia_ = function(p, n, metoda, M) {
  count <- 0
  x <- rbinom(M,n,p)
  przedzialy_l <- binom.confint(x,n,methods=metoda)[5]
  przedzialy_u <- binom.confint(x,n,methods=metoda)[6]

  return (sum((p >= przedzialy_l) & (p<=przedzialy_u))/M)
}

procent_pokrycia = function(n, M=1000){
  metody = c("exact", "asymptotic", "bayes")
  P = seq(0.01,0.99,by = 0.01)
  pokrycie = matrix(0,length(P),3)
  for (m in 1:3) {
  for (p in 1:length(P)){
    pokrycie[p,m] <- procent_pokrycia_(P[p],n, metody[m],M=M)
  }
  }
  return (pokrycie)
}

dlugosc_przedzialu_ = function(p, n, metoda, M) {
  x <- rbinom(M,n,p)
  przedzialy_l <- binom.confint(x,n,methods=metoda)[5]
  przedzialy_u <- binom.confint(x,n,methods=metoda)[6]  
  return (mean((przedzialy_u-przedzialy_l)[,1]))
}

dlugosc_przedzialu = function(n, M=1000){
  metody = c("exact", "asymptotic", "bayes")
  P = seq(0.01,0.99,by = 0.01)
  przedzialy = matrix(0,length(P),3)
  for (m in 1:3) {
    for (p in 1:length(P)){
      przedzialy[p,m] <- dlugosc_przedzialu_(P[p],n, metody[m],M=M)
    }
  }
  return (przedzialy)
}


P = seq(0.01,0.99,by = 0.01)
pokrycie30 = procent_pokrycia(30)
pokrycie100 = procent_pokrycia(100)
pokrycie1000 = procent_pokrycia(1000)

przedzialy30 = dlugosc_przedzialu(30)
przedzialy100 = dlugosc_przedzialu(100)
przedzialy1000 = dlugosc_przedzialu(1000)

colors = c("red", "green", "blue", "black")
par(mfrow=c(1,1))
metody = c("exact", "asymptotic", "bayes")

#30
plot(NULL,xlim=c(0,1), ylim=c(0.2,1), xlab="p-prawdopodobieństwo sukcesu", ylab="% pokrycia", main="n=30")
for (i in 1:length(metody)) {
  lines(P,pokrycie30[,i], col=colors[i])
} 
legend("bottom", c(metody,"0.95"), title="Metoda", fill=colors)
lines(c(0,1),c(0.95,0.95))
#100
plot(NULL,xlim=c(0,1), ylim=c(0.5,1), xlab="p-prawdopodobieństwo sukcesu", ylab="% pokrycia", main="n=100")
for (i in 1:length(metody)) {
  lines(P,pokrycie100[,i], col=colors[i])
} 
legend("bottom", c(metody,"0.95"), title="Metoda", fill=colors)
lines(c(0,1),c(0.95,0.95))
#1000
plot(NULL,xlim=c(0,1), ylim=c(0.6,1), xlab="p-prawdopodobieństwo sukcesu", ylab="% pokrycia", main="n=1000")
for (i in 1:length(metody)) {
  lines(P,pokrycie1000[,i], col=colors[i])
} 
legend("bottom", c(metody,"0.95"), title="Metoda", fill=colors)
lines(c(0,1),c(0.95,0.95))
#dlugosci przedzialow
#30
plot(NULL,xlim=c(0,1), ylim=c(0,0.6), xlab="p-prawdopodobieństwo sukcesu", ylab="średnia dlugość przedziału", main="n=30")
for (i in 1:length(metody)) {
  lines(P,przedzialy30[,i], col=colors[i])
} 
legend("bottom", metody, title="Metoda", fill=colors)

#100
plot(NULL,xlim=c(0,1), ylim=c(0,0.4), xlab="p-prawdopodobieństwo sukcesu", ylab="średnia dlugość przedziału", main="n=100")
for (i in 1:length(metody)) {
  lines(P,przedzialy100[,i], col=colors[i])
} 
legend("bottom", metody, title="Metoda", fill=colors)

#1000
plot(NULL,xlim=c(0,1), ylim=c(0,0.1), xlab="p-prawdopodobieństwo sukcesu", ylab="średnia dlugość przedziału", main="n=1000")
for (i in 1:length(metody)) {
  lines(P,przedzialy1000[,i], col=colors[i])
} 
legend("bottom", metody, title="Metoda", fill=colors)



#zadanie 2
dane <- read.csv2(file="Choroba.csv",header=TRUE)
library(binom)

poziom_ufnosci <- 0.95

metody <-c("exact", "ac", "asymptotic", "wilson", "prop.test", "bayes", "logit", "cloglog", "probit") 

wyniki <- data.frame(Metoda = character(), Dolna = numeric(), Gorna = numeric(), Dlugosc = numeric(), stringsAsFactors = FALSE)

for (metoda in metody) {
  przedzialy_ufnosci <- binom.confint(sum(dane$CHORY_ZD), nrow(dane), method = metoda, conf.level = poziom_ufnosci)
  
  dlugosc_przedzialu <- przedzialy_ufnosci$upper - przedzialy_ufnosci$lower
  
  wyniki <- rbind(wyniki, data.frame(Metoda = metoda, Dolna = przedzialy_ufnosci$lower, Gorna = przedzialy_ufnosci$upper, Dlugosc = dlugosc_przedzialu))
}

print(wyniki)

#zadanie 3
choroba <- read.csv2(file="Choroba.csv",header=TRUE)


# a)
chorzy<-sum(choroba$CHORY_ZD==1) #liczba chorych w populacji
binom.test(chorzy,nrow(choroba),p=0.5,alternative="greater",conf.level = 0.05)
prop.test(chorzy,nrow(choroba),p=0.5,correct=TRUE,alternative="greater",conf.level = 0.05)
prop.test(chorzy,nrow(choroba),p=0.5,correct=FALSE,alternative="greater",conf.level = 0.05)

# b)

chorzy_s1<- sum(choroba$CHORY_ZD==1 & choroba$SEKTOR==1)
chorzy_s2 <- sum(choroba$CHORY_ZD==1 & choroba$SEKTOR==2)

sektor1<-sum(choroba$SEKTOR==1)
sektor2<-sum(choroba$SEKTOR==2)

prop.test(c(chorzy_s1,chorzy_s2),c(sektor1,sektor2),alternative='t',correct=TRUE)
prop.test(c(chorzy_s1,chorzy_s2),c(sektor1,sektor2),alternative='t',correct=FALSE)

# c)
chorzy_sred<-sum(choroba$CHORY_ZD==1 & choroba$STATUS==2) #liczba chorych ze średnim statusem ekonomicznym
sredni_status<-sum(choroba$STATUS==2)
test_c<-binom.test(chorzy_sred,sredni_status,p=0.5,alternative="greater")
test_c

print(test_c$p.value) 

chorzy_s1_s<- sum(choroba$CHORY_ZD==1 & choroba$SEKTOR==1 & choroba$STATUS==2)
chorzy_s2_s <- sum(choroba$CHORY_ZD==1 & choroba$SEKTOR==2 & choroba$STATUS==2)

sektor1_s<-sum(choroba$SEKTOR==1& choroba$STATUS==2)
sektor2_s<-sum(choroba$SEKTOR==2& choroba$STATUS==2)

prop.test(c(chorzy_s1_s,chorzy_s2_s),c(sektor1_s,sektor2_s),alternative='t')
prop.test(c(chorzy_s1_s,chorzy_s2_s),c(sektor1_s,sektor2_s),alternative='t',correct=FALSE)


#zadanie 4a
binom.test_p = function(x,n,p=0.5, alternative, conf.level=0.95){
  return (unlist((binom.test(x,n,p=0.5, alternative="t", conf.level=conf.level)$p.value)))
}

prop.test_p = function(x,n,p=0.5, alternative, conf.level=0.95, correct=TRUE){
  return (unlist((prop.test(x,n,p=0.5, alternative="t", conf.level=conf.level, correct=correct)$p.value)))
}

blad1rodzaju = function(n,N=2000,conf=0.95){
  x = rbinom(N, n, 0.5)
  #liczenia p-wartosci dla realizacji
  p1_ <- lapply(x, FUN=binom.test_p, n=n, p=0.5, alternative="t", conf.level=conf)
  p2_ <- lapply(x, FUN=prop.test_p, n=n, p=0.5, alternative="t", conf.level=conf, correct=TRUE)
  p3_ <- lapply(x, FUN=prop.test_p, n=n, p=0.5, alternative="t", conf.level=conf, correct=FALSE)
  
  #sprawdzanie czy p-wartosc <0.05
  p1 <- sum(p1_<1-conf)/N
  p2 <- sum(p2_<1-conf)/N
  p3 <- sum(p3_<1-conf)/N
  
  return (c(p1, p2,p3))
}

blad1_30 = blad1rodzaju(30)
blad1_100 = blad1rodzaju(100)
blad1_1000 = blad1rodzaju(1000)


#ZADANIE 4b
binom.test_p = function(x,n,p=0.5, alternative, conf.level=0.95){
  return (unlist((binom.test(x,n,p=0.5, alternative="t", conf.level=conf.level)$p.value)))
}

prop.test_p = function(x,n,p=0.5, alternative, conf.level=0.95, correct=TRUE){
  return (unlist((prop.test(x,n,p=0.5, alternative="t", conf.level=conf.level, correct=correct)$p.value)))
}

moctestu_ = function(p,n,N,conf=0.95){ #dla ustalonego p
  x = rbinom(N, n, p)
  p1_ <- lapply(x, FUN=binom.test_p, n=n, p=0.5, alternative="t", conf.level=conf)
  p2_ <- lapply(x, FUN=prop.test_p, n=n, p=0.5, alternative="t", conf.level=conf, correct=TRUE)
  p3_ <- lapply(x, FUN=prop.test_p, n=n, p=0.5, alternative="t", conf.level=conf, correct=FALSE)
  
  p1 <- sum(p1_<0.05)/N
  p2 <- sum(p2_<0.05)/N
  p3 <- sum(p3_<0.05)/N
  
  return (c(p1,p2,p3))
}

moctestu = function(n, N=500,conf=0.95){ # dla wielu p
  P = c(seq(0.01,0.49,by = 0.01),seq(0.51,0.99,by = 0.01))
  blad = matrix(0,length(P),3)
  for (p in 1:length(P)){
      blad[p,] <- moctestu_(P[p],n, N=N,conf = conf)
  }
  return (blad)
}

colors = c("red", "green", "blue", "black")
pch = c(15,17,20)
testy=c("binom.test", "prop.test z poprawką na ciągłość", "prop.test bez poprawki na ciągłość")

P = c(seq(0.01,0.49,by = 0.01),seq(0.51,0.99,by = 0.01))


P#30
blad30 = moctestu(30,N=500)
plot(NULL, ylim=c(0,1),xlim=c(0,1), xlab="p-prawdopodobieństwo sukcesu", ylab="moc testu", main="n=30")
for (i in 1:3) {
  points(P,blad30[,i], col=colors[i],pch=pch[i])
  lines(P,blad30[,i], col=colors[i])
}
lines(c(0,1),c(0.05,0.05))
legend("bottomleft", c(testy,0.05), title="użyta funkcja",pch=pch, col=colors)


#100
blad100 = moctestu(100,N=500)
plot(NULL, ylim=c(0,1),xlim=c(0,1), xlab="p-prawdopodobieństwo sukcesu", ylab="moc testu", main="n=100")
for (i in 1:3) {
  points(P,blad100[,i], col=colors[i],pch=pch[i])
  lines(P,blad100[,i], col=colors[i])
}
lines(c(0,1),c(0.05,0.05))
legend("bottomleft", c(testy,0.05), title="użyta funkcja",pch=pch, col=colors)

#1000
blad1000 = moctestu(1000,N=500)
plot(NULL, ylim=c(0,1),xlim=c(0,1), xlab="p-prawdopodobieństwo sukcesu", ylab="moc testu", main="n=1000")
for (i in 1:3) {
  points(P,blad1000[,i], col=colors[i],pch=pch[i])
  lines(P,blad1000[,i], col=colors[i])
}
l(c(0,1),c(0.05,0.05))
legend("bottomleft", c(testy,0.05), title="użyta funkcja",pch=pch, col=colors)


