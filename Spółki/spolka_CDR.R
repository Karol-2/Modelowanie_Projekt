#ane<- read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/Modelowanie_Projekt_1-main/Spółki/cdr_d.csv")
dane<- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/cdr_v2.csv")


library(ggplot2)
library(moments)
library(fitdistrplus)

#View(dane)

class(dane)

kurs_zamkniecia <- dane$Zamknięcie

par(mfrow = c(1,1))

# 1
plot(kurs_zamkniecia, main="Wykres kursów zamknięcia akcji CD Projekt", xlab="Dzień", ylab="Kurs zamknięcia")
grid()
hist(kurs_zamkniecia, prob=TRUE, main="Histogram", xlab="Kurs zamknięcia", ylab="Gestość")
# 2
mean(kurs_zamkniecia)
sd(kurs_zamkniecia)
skewness(kurs_zamkniecia)
kurtosis(kurs_zamkniecia)

# 3
plot.legend <- c('norm','lognorm','gamma')

estymator_normalny <- fitdist(kurs_zamkniecia,"norm",method = "mle")
estymator_normalny

estymator_lognorm <- fitdist(kurs_zamkniecia,"lnorm",method = "mle")
estymator_lognorm

estymator_gamma <- fitdist(kurs_zamkniecia,"gamma",method = "mle")
estymator_gamma

# 
par(mfrow = c(2,2))

denscomp(list(estymator_normalny,estymator_lognorm,estymator_gamma), legendtext = plot.legend)
cdfcomp(list(estymator_normalny,estymator_lognorm,estymator_gamma), legend = plot.legend)
qqcomp(list(estymator_normalny,estymator_lognorm,estymator_gamma), legend = plot.legend)
ppcomp(list(estymator_normalny,estymator_lognorm,estymator_gamma), legend = plot.legend)



#kryteria wyboru - wychodzi że LOGNORM -3 ,gamma - 2
gofstat(list(estymator_normalny, estymator_lognorm, estymator_gamma),  fitnames = plot.legend)

# 5
N <- 10000
n <- length(kurs_zamkniecia)
n

Dln <- c()

 meanlog<-estymator_lognorm$estimate[1]
 sdlog <- estymator_lognorm$estimate[2]

for (i in 1:N) {
  Yln <- rlnorm(n, meanlog, sdlog)
  
  Dln[i] <- ks.test(Yln, plnorm, meanlog,sdlog, exact = TRUE)$statistic
}

dn_ln <- ks.test(kurs_zamkniecia, plnorm, meanlog, sdlog, exact = TRUE)$statistic
dn_ln

par(mfrow = c(1,1))

hist(Dln, prob = T, xlim=c(0, 0.17))
points(dn_ln, 0, pch = 19, col = 2)


p_value <- length(Dln[Dln>dn_ln])/N; 
p_value
alpha <- 0.05
p_value <= alpha
#Wartosc p-value jest mniejsza od przyjetego poziomu istotnosci
#Hipoteze o rownosci dystrybuant odrzucamy

