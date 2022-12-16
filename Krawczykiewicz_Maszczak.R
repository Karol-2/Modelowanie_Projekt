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

# DODAĆ KOD Z PIERWSZEJ CZĘŚCI PROJEKTU !!!

# =========================================================
#                      CZĘŚĆ DRUGA
# =========================================================


bit11_zamkniecie <- dane_11B$Zamknięcie
cdp_zamkniecie <- dane_CDP$Zamknięcie

diff_bit11<-diff(log(bit11_zamkniecie),lag=1)
diff_cdp<-diff(log(cdp_zamkniecie),lag=1)

df <- data.frame(bit11=diff_bit11,cdp=diff_cdp )

#--------------------------
# A COŚ W TYCH DWÓCH PIERWSZYCH PODPUNKTACH JEST ZJEBANE

# 1. wykres rozrzutu z histogramami rozkładów przegowych
p <-  ggplot(df, aes(x=bit11, y=cdp)) + geom_point()
ggMarginal(p, type="histogram")

# 2. BRAKUJE JEDNEGO!!! - współczynnik korelacji
mu <- colMeans(df);mu; # wektor średnich  
Sigma <- cov(df); Sigma; # macierz kowariancji
P <- cor(df);P; # macierz korelacji
# czy 0,32 będzie wtedy współczynikiem korelacji?


# 3.GĘSTOŚĆ 

#obliczamy wartosci gestosci na siatce punktow [-x0,x0] x [-y0,y0]
#rozmiar siatki dobieramy z reguly trzech sigm, dla rozkladu normalnego
s1 <- s2 <- 1 #odchylenia standardowe 
x     <- seq(-3*s1, 3*s1, 0.25) 
y     <- seq(-3*s2, 3*s2, 0.25)

f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)  #sprawdx co oblicza funkcja outer


#dokladniejszy wykres
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")


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

