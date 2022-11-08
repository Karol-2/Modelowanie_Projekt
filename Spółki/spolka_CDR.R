#dane<- read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/cdr_d.csv")
dane<- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/cdr_d.csv")


library(ggplot2)
library(moments)
library(fitdistrplus)

#View(dane)

class(dane)

cena_zamkniecia <- dane$Zamkniecie

par(mfrow = c(3,1))
# 1
plot(cena_zamkniecia)

hist(cena_zamkniecia, prob=TRUE)
# 2
mean(cena_zamkniecia)
sd(cena_zamkniecia)
skewness(cena_zamkniecia)
kurtosis(cena_zamkniecia)
# 3
plot.legend <- c('norm','lognorm','gamma')

normalny <- fitdist(cena_zamkniecia,"norm")
normalny

lognorm <- fitdist(cena_zamkniecia,"lnorm")
lognorm

gamma <- fitdist(cena_zamkniecia,"gamma")
gamma

denscomp(list(normalny,lognorm,gamma), legendtext = plot.legend)

