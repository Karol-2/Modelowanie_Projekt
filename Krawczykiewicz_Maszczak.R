library(ggplot2)
library(ggExtra)
library(mnormt)
library(MASS)
library(QRM)
library(evir)

#dane_CDP<- read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/Spółki/cdr.csv")
#dane_11B<-read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/Spółki/11bit.csv")

dane_CDP<- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/cdr.csv")
dane_11B<- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/11bit.csv")

# DODAĆ KOD Z PIERWSZEJ CZĘŚCI PROJEKTU?

# =========================================================
#                      CZĘŚĆ DRUGA
# =========================================================

# SPRAWDZIŁBYM CZY TO NA PEWNO DOBRZE LICZY !!!
bit11_zamkniecie <- dane_11B$Zamknięcie
cdp_zamkniecie <- dane_CDP$Zamknięcie

bit11_dlugosc <- length(bit11_zamkniecie);
bit11_log_zwroty <- log(bit11_zamkniecie[-1]/bit11_zamkniecie[-bit11_dlugosc])

cdp_dlugosc <- length(cdp_zamkniecie);
cdp_log_zwroty <- log(cdp_zamkniecie[-1]/cdp_zamkniecie[-cdp_dlugosc])

#diff(log(cd_zamkniecie),lag=1) - to się może przydać maybe

df <- data.frame(bit11=bit11_log_zwroty,cdp=cdp_log_zwroty )

#--------------------------
# A

# 1. wykres rozrzutu z histogramami rozkładów przegowych
p <-  ggplot(df, aes(x=bit11, y=cdp)) + geom_point()
ggMarginal(p, type="histogram")

# 2. BRAKUJE JEDNEGO!!! - współczynnik korelacji
mu <- colMeans(df);mu; # wektor średnich  
Sigma <- cov(df); Sigma; # macierz kowariancji
P <- cor(df);P; # macierz korelacji


# 3.GĘSTOŚĆ NIE MA !!!


#----------------------------

# B
# 1. wykresy rozrzutu rozkładu normalnego i naszych danych
n <- nrow(df); n

set.seed(100)
Z <- MASS::mvrnorm(n,mu=mu,Sigma=Sigma)

par(mfrow=c(1,2))
plot(df, xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))
plot(Z,xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))
par(mfrow=c(1,1))

# 2. Mahalanobis
dM <- mahalanobis(df,mu,Sigma)

hist(dM,prob=TRUE)

n <- dim(df)[1]; n
alpha <- ppoints(n)
q_emp <- quantile(dM,alpha)
q_teo <- qchisq(alpha,df=2)

plot(q_emp,q_teo,pch=19)
abline(a=0,b=1,col=2)

ks.test(dM,'pchisq',2)
# p-value < 5% - odrzucamy

# 3.DODATKOWE JEŚLI JESTEŚMY CHĘTNI !!!

