cdr <- read.csv("C:/Studia/Modelowanie Matematyczne/Modelowanie_Projekt_1/Spółki/cdr.csv")
bit11 <- read.csv("C:/Studia/Modelowanie Matematyczne/Modelowanie_Projekt_1/Spółki/11bit.csv")

library(ggplot2)
library(moments)
library(fitdistrplus)
library(MASS)
library(survival)

####################################################
# CDR
#

class(cdr)

kurs_zamkniecia <- cdr$Zamknięcie

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

#####################################################
# 11 bit
#

bit11$Data <- as.Date(bit11$Data)
class(bit11$Data)

cena_zamkniecia <- bit11$Zamknięcie

#1
par(mfrow = c(1, 1))

plot(cena_zamkniecia, main="Wykres kursów zamknięcia akcji 11 bit studios", xlab="Dzień", ylab="Kurs zamknięcia")
grid()
hist(cena_zamkniecia, main="Histogram", xlab="Kurs zamknięcia", ylab="Gęstość", prob=TRUE)

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
shape <- fw$estimate[1]; shape
scale <- fw$estimate[2]; scale

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