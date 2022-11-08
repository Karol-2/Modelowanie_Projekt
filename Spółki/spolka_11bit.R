dane <- read.csv("//NAS1/home/pmaszczak/Desktop/modelowanie/a11bit.csv")
library(moments)
library(ggplot2)
library(fitdistrplus)

dane$Data <- as.Date(dane$Data)
class(dane$Data)

cena_zamkniecia <- dane$Zamknięcie

#1
par(mfrow = c(6, 1))

plot(cena_zamkniecia, main="Wykres cen spółki 11bit 2020-2021", xlab="numer dnia", ylab="cena zamknecia")

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

