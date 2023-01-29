library(ggplot2)
library(boot)  


#cdr <- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/cdr.csv")
#bit11 <- read.csv("D:/Studia/3sem/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/11bit.csv")

cdr <- read.csv("C:/Studia/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/cdr.csv")
bit11 <- read.csv("C:/Studia/Modelowanie matematyczne/Modelowanie_Projekt_1/Spółki/11bit.csv")


kurs_zamkniecia <- cdr$Zamknięcie
cena_zamkniecia <- bit11$Zamknięcie
bit11_zamkniecie <- cena_zamkniecia
cdp_zamkniecie <- kurs_zamkniecia
diff_bit11<-diff(log(bit11_zamkniecie),lag=1)
diff_cdp<-diff(log(cdp_zamkniecie),lag=1)

Y_11B <- diff_bit11
X_CDR <- diff_cdp; 

df <- data.frame(Y_11B=Y_11B,X_CDR=X_CDR)


#estymatory wspolczynnikow
beta1 <- cov(X_CDR,Y_11B)/var(X_CDR)
beta0 <- mean(X_CDR)-mean(Y_11B)*beta1
beta1; beta0

#linia regresji na  wykresie 
#qplot(X_CDR, Y_11B, data = df,
  #    main = "Prosta regresji") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #geom_point(colour = "blue", size = 1.5) +
 # #geom_abline(intercept = beta0, slope = beta1, color="red",size=1)

#funkcja 'lm'


model.lm <- lm(Y_11B~X_CDR,data=df)
model.lm

sum <- summary(model.lm)
sum

#Przyklad 1.c (test istotnosci wspolczynnikow bo, b1)
#===========
#Obliczamy wartosci statystyki T oraz p-value

#wspolczynniki modelu 
coef <- model.lm$coefficients  #lub coef(model.lm)
coef
beta0 <- coef[[1]]
beta1 <- coef[[2]]
beta0; beta1 

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

#Przyklad 1.d  (analiza reszt)
#===========

#reszty (residuals) 
reszty <- model.lm$residuals

#histogram i qq-ploty
hist(reszty)

qqnorm(reszty)
qqline(reszty,col=2)

m <- mean(reszty)
s <- sd(reszty)
m;s

ks.test(reszty,'pnorm',m,s)

#test Shapiro-Wilka
shapiro.test(reszty)

#p-value=0.1381, na poziomie 5% nie ma podstaw
#do odrzucenia hipotezy o normalnosci rozkladu reszt

#RSE - blad standardowy reszt
RSE <- sqrt(sum(reszty^2)/(length(X_CDR)-2))
RSE

#Przyklad 1.e Predykcja (pozostawiamy wyestymowane b0)
#=========
#Ile waży serce kotka, którego model jest rowna sredniej z badanej probki kotkow?
m <- mean(X_CDR)

beta0+beta1*m

#Przyklad 1.f Ponowna regresja i predykcja, przy b0=0
#=========

Y_11B <- diff_bit11
X_CDR <- diff_cdp; 

df <- data.frame(Y_11B=Y_11B,X_CDR=X_CDR)

model.lm2 <- lm(Y_11B~X_CDR-1,data=df) #CHYBA GIT
model.lm2 #model 2
model.lm  #model 1

sum1 <- summary(model.lm)
sum2 <- summary(model.lm2)
sum1; sum2 

#Ile waży serce kotka, którego model jest rowna sredniej z badanej probki kotkow?
m <- mean(X_CDR)

beta1_model2 <- model.lm2$coefficients

beta1_model2*m  #predykcja model 2
beta0+beta1*m   #predykcja model 1

#Przyklad 1.g (Predykcja i przedzialy ufnosci dla predykcji)
#=========
nowa.model <- data.frame(X_CDR=m)

nowa.model;
model.lm;

predict(model.lm, nowa.model, interval="confidence")  #model 1
predict(model.lm2, nowa.model, interval="confidence") #model 2


# porównanie modeli przez chat
#Oba modele to modele regresji liniowej z jednym zmiennym predykcyjnym, Y_11B. Jedyną różnicą jest to, że pierwszy model zawiera wyraz wolny, podczas gdy drugi model go nie ma (Y_11B - 1 w formule).

#Na podstawie danych wynikowych, współczynniki, wartości t, wartości p, R-kwadrat i statystyka F są bardzo podobne dla obu modeli, co sugeruje, że zawarcie lub brak wyrazu wolnego nie ma istotnego wpływu na wyniki. Dlatego można stwierdzić, że oba modele są równoważne pod względem siły ich związku z zmienną objaśnianą oraz ich zdolności do jej przewidywania.

#Jeśli miałbym wybrać jeden z tych modeli, to powiedziałbym, że drugi model (X_CDR ~ Y_11B - 1) jest nieco lepszy, ponieważ ma nieco wyższą wartość skorygowanego współczynnika R-kwadratu oraz nieco niższy błąd standardowy resztowy. Wartość skorygowanego R-kwadratu wskazuje, jak dobrze model pasuje do danych, biorąc pod uwagę liczbę zmiennych predykcyjnych. Większa wartość oznacza lepsze dopasowanie. Błąd standardowy resztowy mierzy średnią odchylenie reszt (błędy prognozy) od rzeczywistych wartości, a niższa wartość oznacza lepsze dopasowanie.

#Warto zauważyć, że te różnice są bardzo małe i dlatego nie byłyby istotne w praktyce. Wybór modelu do użycia powinien zależeć od konkretnego pytania badawczego i kontekstu, w jakim zbierano dane.


