#cdr <- read.csv("C:/Studia/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/cdr.csv")
#bit11 <- read.csv("C:/Studia/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/11bit.csv")

cdr <- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/cdr.csv")
bit11 <- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/11bit.csv")


library(ggplot2)
library(moments)
library(fitdistrplus)
library(MASS)
library(survival)
library(ggExtra)
library(mnormt)
library(QRM)
library(evir)

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




#####################################################
# Praca 2. Analiza łącznego rozkładu log-zwrotów
#




bit11_zamkniecie <- cena_zamkniecia
cdp_zamkniecie <- kurs_zamkniecia

diff_bit11<-diff(log(bit11_zamkniecie),lag=1)
diff_cdp<-diff(log(cdp_zamkniecie),lag=1)

df <- data.frame(bit11=diff_bit11,cdp=diff_cdp)

#--------------------------


# 1. wykres rozrzutu z histogramami rozkładów brzegowych
p <-  ggplot(df, aes(x=bit11, y=cdp)) + geom_point()
ggMarginal(p, type="histogram")



# 2. Estymatory
mu <- colMeans(df);mu; # wektor średnich
covariance <- cov(df$bit11, df$cdp); covariance; # kowariancja
correlation <- cor(df$bit11, df$cdp); correlation; # współczynnik korelacji
Sigma <- cov(df); Sigma; # macierz kowariancji
correlation_matrix <- cor(df); correlation_matrix; # macierz korelacji


# 3 Wykres gestosci rozkladu
s1 <- sqrt(Sigma[1])
s2 <- sqrt(Sigma[4])
x     <- seq(-3*s1, 3*s1, 0.005) 
y     <- seq(-3*s2, 3*s2, 0.005)
f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)
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
# p-value < 5% - 

#---------------------------- częśc 3 --------------

# A

#CDR
Xn_CDR <- diff_cdp; 
u_CDR<-mean(Xn_CDR) ;u_CDR
Sn_CDR <- sd(Xn_CDR) ; Sn_CDR
n <- length(Xn_CDR) ; n
alfa <- 0.05

kwantyl <- qnorm(1-alfa/2, mean = 0, sd = 1) #kwantyl rzedu p rozkladu N(0,1)

lewy_CDR <-  u_CDR - kwantyl * Sn_CDR/sqrt(n) ; lewy_CDR
prawy_CDR <-  u_CDR + kwantyl * Sn_CDR/sqrt(n) ; prawy_CDR


#11B
Xn_11B <- diff_bit11
u_11B<-mean(Xn_11B) ;u_11B
Sn_11B <- sd(Xn_11B) ; u_11B
n <- length(Xn_11B) ; n
alfa <- 0.05


lewy_11B <-  u_11B - kwantyl * Sn_11B/sqrt(n) ; lewy_11B
prawy_11B <-  u_11B + kwantyl * Sn_11B/sqrt(n) ; prawy_11B

# B
#1
b1 <- cov(Xn_CDR,Xn_11B)/var(Xn_CDR); b1
b0 <- mean(Xn_CDR)-mean(Xn_11B)*b1; b0

df <- data.frame(bit11=diff_bit11,cdp=diff_cdp)


ggplot(data = df, aes(x = Xn_CDR, y = Xn_11B)) +
  geom_point(colour = "blue", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size=1) +
  ggtitle("nazwa wykresu") +
  theme(plot.title = element_text(hjust = 0.5))

#wspołczynniki

model.lm <-lm(Xn_CDR~Xn_11B,data=df)
model.lm

sum <- summary(model.lm)

#Obliczamy wartosci statystyki T oraz p-value

#wspolczynniki modelu 
coef <- model.lm$coefficients 
b0 <- coef[[1]]
b1 <- coef[[2]]
b0; b1 

#odchylenia standardowe estymatorow beta0, beta1
coef_all <- sum$coefficients
coef_all 

se.beta0 <- coef_all[1,2]
se.beta1 <- coef_all[2,2]
se.beta0; se.beta1

#wartosci statystyki T (t value) 
t0 <- beta0/se.beta0
t1 <- beta1/se.beta1

t0; t1

#p-value (p = P(|T|>t0), p = P(|T|>t1))
2*(1-pt(abs(t0),95))
2*(1-pt(abs(t1),95))

#warosci p = P(|T|>t0), p = P(|T|>t1) sa mniejsze od 5% zatem hipoteze
#ze wspolczynniki sa rowne zero, odrzucamy na tym poziomie istotnosci

# p value t0 =0.5598307 -  hipoteze że ten współczynnik = 0 przyjmujemy
# p value t1 = 2.205374e-06 - hipoteze że ten współczynnik = 0 odrzucamy

