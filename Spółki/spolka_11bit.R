dane <- read.csv("C:/Studia/Modelowanie Matematyczne/Modelowanie_Projekt_1/Spółki/11bit.csv")
library(moments)
library(ggplot2)
library(fitdistrplus)
library(MASS)
library(survival)

dane$Data <- as.Date(dane$Data)
class(dane$Data)

cena_zamkniecia <- dane$Zamknięcie

#1
par(mfrow = c(1, 1))

plot(cena_zamkniecia, main="Wykres kursow zamkniecia akcji 11bit", xlab="Notowany dzien", ylab="Cena zamkniecia")

hist(cena_zamkniecia, main="Histogram", xlab="Cena zamkniecia", ylab="Gestosc", prob=TRUE)

#2
srednia <- mean(cena_zamkniecia); srednia

odchylenie_st <- sd(cena_zamkniecia); odchylenie_st

skosnosc <- skewness(cena_zamkniecia); skosnosc

kurtoza <- kurtosis(cena_zamkniecia); kurtoza

#3
norm <- fitdist(cena_zamkniecia, "norm"); norm

lnorm <- fitdist(cena_zamkniecia, "lnorm"); lnorm

weib <- fitdist(cena_zamkniecia, "weibull"); weib

#4
plot.legend <- c('log-norm', 'norm', 'weibull')

fn <- fitdist(cena_zamkniecia, "norm")
fln <- fitdist(cena_zamkniecia, "lnorm")
fw <- fitdist(cena_zamkniecia, "weibull")

funkcje <- list(fn, fln, fw)
legenda = c("normalny", "log-normalny", "weibull")

par(mfrow = c(2, 2))

cdfcomp(funkcje, legend = legenda)
denscomp(funkcje, legend = legenda)
qqcomp(funkcje, legend = legenda)
ppcomp(funkcje, legend = legenda)
gofstat(funkcje, fitnames = legenda)
# Ceny zamkniecia najlepiej opisuje rozklad Weibulla

par(mfrow = c(1, 1))
# 5

N <- 10000
n <- length(cena_zamkniecia)

Dw <- c()
shape <- fw$estimate[1]
scale <- fw$estimate[2]

for (i in 1:N) {
  Yw <- rweibull(n, shape=shape, scale=scale)
  
  Dw[i] <- ks.test(Yw, pweibull, scale=scale, shape=shape, exact = TRUE)$statistic
}

dn_w <- ks.test(cena_zamkniecia, pweibull, scale=scale, shape=shape, exact = TRUE)$statistic; dn_w

  hist(Dw, prob=T, xlim=c(0, 0.115), ylim=c(0, 40))
points(dn_w, 0, pch = 19, col = 2)

p_value <- length(Dw[Dw>dn_w])/N; p_value
alpha <- 0.05
p_value <= alpha
#Hipoteza o równości rozkładów odrzucona, pvalue < 5%
