#dane<- read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/cdr_d.csv")
dane<- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/cdr_d.csv")


library(ggplot2)
library(moments)
library(fitdistrplus)

#View(dane)

class(dane)

kurs_zamkniecia <- dane$Zamkniecie

par(mfrow = c(1,1))

# 1
plot(kurs_zamkniecia)

hist(kurs_zamkniecia, prob=TRUE)

# 2
mean(kurs_zamkniecia)
sd(kurs_zamkniecia)
skewness(kurs_zamkniecia)
kurtosis(kurs_zamkniecia)

# 3
plot.legend <- c('norm','lognorm','gamma')

estymator_normalny <- fitdist(kurs_zamkniecia,"norm")
estymator_normalny

estymator_lognorm <- fitdist(kurs_zamkniecia,"lnorm")
estymator_lognorm

estymator_gamma <- fitdist(kurs_zamkniecia,"gamma")
estymator_gamma

# 4
denscomp(list(estymator_normalny,estymator_lognorm,estymator_gamma), legendtext = plot.legend)
cdfcomp(list(estymator_normalny,estymator_lognorm,estymator_gamma), legend = plot.legend)
qqcomp(list(estymator_normalny,estymator_lognorm,estymator_gamma), legend = plot.legend)

#kryteria wyboru - wychodzi że LOGNORM
gofstat(list(estymator_normalny, estymator_lognorm, estymator_gamma),  fitnames = plot.legend)

# 5
N <- 10000
n <- 100

Dn <- c()

for (i in 1:N) {
  Yln <- rlnorm(n, estymator_lognorm$estimate[1], estymator_lognorm$estimate[2])
  
  Dn[i] <- ks.test(Yln, plnorm, estymator_lognorm$estimate[1],estymator_lognorm$estimate[2], exact = TRUE)$statistic
}

dn <- ks.test(kurs_zamkniecia, plnorm, estymator_lognorm$estimate[1], estymator_lognorm$estimate[2], exact = TRUE)$statistic

hist(Dn, prob = T)
points(dn, 0, pch = 19, col = 2)

p_value <- length(Dn[Dn>dn])/N; p_value
alpha <- 0.05
p_value >= alpha
# Odrzucamy test zgodnosci 

