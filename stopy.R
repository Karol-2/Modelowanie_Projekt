library(ggplot2)
library(ggExtra)
library(mnormt)
library(MASS)
library(QRM)
library(evir)

#View(dane_CDP)

dane_CDP<- read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/Spółki/cdr.csv")
dane_11B<-read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/Spółki/11bit.csv")

bit11_zamkniecie <- dane_11B$Zamknięcie
cdp_zamkniecie <- dane_CDP$Zamknięcie

bit11n <- length(bit11_zamkniecie);
bit11lrest <- log(bit11_zamkniecie[-1]/bit11_zamkniecie[-bit11n])
#bit11lrest

cdpn <- length(cdp_zamkniecie);
cdplrest <- log(cdp_zamkniecie[-1]/cdp_zamkniecie[-cdpn])
#cdplrest

#diff(log(cd_zamkniecie),lag=1)

df <- data.frame(bit11=bit11lrest,cdp=cdplrest)
# A
p <-  ggplot(df, aes(x=bit11, y=cdp)) + geom_point()
ggMarginal(p, type="histogram")

# B
mu <- colMeans(df)
Sigma <- cov(df)
P <- cor(df)
mu; Sigma; P

n <- nrow(df); n

set.seed(100)
Z <- MASS::mvrnorm(n,mu=mu,Sigma=Sigma)

par(mfrow=c(1,2))
plot(df, xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))
plot(Z,xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))

dM <- mahalanobis(df,mu,Sigma)

par(mfrow=c(1,1))
hist(dM,prob=TRUE)

n <- dim(df)[1]; n
alpha <- ppoints(n)
q_emp <- quantile(dM,alpha)
q_teo <- qchisq(alpha,df=2)

plot(q_emp,q_teo,pch=19)
abline(a=0,b=1,col=2)



ks.test(dM,'pchisq',2)


