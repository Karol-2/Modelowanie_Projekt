library(ggplot2)
library(ggExtra)
library(mnormt)
library(MASS)
library(QRM)
library(evir)



dane_CDP<- read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/Spółki/cdr.csv")
dane_11B<-read.csv("//NAS1/home/kkrawczykiewicz/Desktop/modelowanie/Spółki/11bit.csv")

bit11_zamkniecie <- dane_11B$Zamknięcie
cdp_zamkniecie <- dane_CDP$Zamknięcie

bit11n <- length(bit11_zamkniecie);
bit11lrest <- log(bit11_zamkniecie[-1]/bit11_zamkniecie[-bit11n])
bit11lrest

cdpn <- length(cdp_zamkniecie);
cdplrest <- log(cdp_zamkniecie[-1]/cdp_zamkniecie[-cdpn])
cdplrest




#View(dane_CDP)


df <- data.frame(bit11=bit11lrest,cdp=cdplrest)

p <-  ggplot(df, aes(x=bit11, y=cdp)) + geom_point()
ggMarginal(p, type="histogram")
