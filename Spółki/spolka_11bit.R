dane <- read.csv("//NAS1/home/pmaszczak/Desktop/Modelowanie_Projekt_1/Spółki/11bit.csv")
library(moments)
library(ggplot2)
library(fitdistrplus)

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

gamma <- fitdist(cena_zamkniecia, "gamma"); gamma

#4
plot.legend <- c('log-norm', 'norm', 'gamma')

fn <- fitdist(cena_zamkniecia, "norm")
fln <- fitdist(cena_zamkniecia, "lnorm")
fg <- fitdist(cena_zamkniecia, "gamma")

funkcje <- list(fn, fln, fg)
legenda = c("normalny","log-normalny","gamma")

cdfcomp(funkcje, legend = legenda)
denscomp(funkcje, legend = legenda)
qqcomp(funkcje, legend = legenda)
ppcomp(funkcje, legend = legenda)
gofstat(funkcje, fitnames = legenda)
# Ceny zamkniecia najlepiej opisuje rozklad log-normalny

# 5

N <- 10000
n <- length(cena_zamkniecia)

Dln <- c()

for (i in 1:N) {
  Yln <- rlnorm(n, fln$estimate[1], fln$estimate[2])
  
  Dln[i] <- ks.test(Yln, plnorm, fln$estimate[1],fln$estimate[2], exact = TRUE)$statistic
}

dn_ln <- ks.test(cena_zamkniecia, plnorm, fln$estimate[1], fln$estimate[2], exact = TRUE)$statistic
dn_ln
# Wartosc statystyki Dln - dn_ln

hist(Dln, prob=T, xlim=c(0, 0.15), ylim=c(0, 40))
points(dn_ln, 0, pch = 19, col = 2)

p_value_ln <- length(Dln[Dln>dn_ln])/N; p_value_ln
alpha <- 0.05
p_value_ln <= alpha
# Hipoteza odrzucona
