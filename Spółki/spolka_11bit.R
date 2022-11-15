#dane <- read.csv("//NAS1/home/pmaszczak/Desktop/modelowanie/a11bit.csv")
dane <- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/11bit.csv")
library(moments)
library(ggplot2)
library(fitdistrplus)

dane$Data <- as.Date(dane$Data)
class(dane$Data)

cena_zamkniecia <- dane$Zamknięcie

#1
par(mfrow = c(1, 1))

plot(cena_zamkniecia)

hist(cena_zamkniecia, main="Histogram", xlab="cena zamkniecia", ylab="gestosc", prob=TRUE)

#2
srednia <- mean(cena_zamkniecia)
srednia

odchylenie_st <- sd(cena_zamkniecia)
odchylenie_st

skosnosc <- skewness(cena_zamkniecia)
skosnosc

kurtoza <- kurtosis(cena_zamkniecia)
skosnosc

#3
norm <- fitdist(cena_zamkniecia, "norm")
norm

lnorm <- fitdist(cena_zamkniecia, "lnorm")
lnorm

gamma <- fitdist(cena_zamkniecia, "gamma")
gamma

#4
plot.legend <- c('log-norm', 'norm', 'gamma')

fn <- fitdist(cena_zamkniecia, "norm")
fln <- fitdist(cena_zamkniecia, "lnorm")
fg <- fitdist(cena_zamkniecia, "gamma")

denscomp(list(fn, fln, fg), legendtext = plot.legend)
cdfcomp(list(fn, fln, fg), legendtext = plot.legend)
qqcomp(list(fn, fln, fg), legendtext = plot.legend)
ppcomp(list(fn, fln, fg), legendtext = plot.legend)

gofstat(list(fn, fln, fg),  fitnames = plot.legend)

# 5

N <- 10000
n <- 100

Dn <- c()

for (i in 1:N) {
  Yln <- rlnorm(n, fln$estimate[1], fln$estimate[2])
  
  Dn[i] <- ks.test(Yln, plnorm, fln$estimate[1],fln$estimate[2], exact = TRUE)$statistic
}

dn <- ks.test(cena_zamkniecia, plnorm, fln$estimate[1], fln$estimate[2], exact = TRUE)$statistic

hist(Dn, prob = T)
points(dn, 0, pch = 19, col = 2)

p_value <- length(Dn[Dn>dn])/N; p_value
alpha <- 0.05
# JEST GIT, POTWIERDAZAM