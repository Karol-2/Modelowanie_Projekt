# Modelowanie_Projekt
📈 Projekt analizujący ceny akcji spółek CD Projekt Red i 11 bit studios.

| ![11B-kursy.png](https://github.com/Piterson25/Modelowanie_Projekt/blob/main/Wykresy/11B-kursy.png) | ![QQploty.png](https://github.com/Piterson25/Modelowanie_Projekt/blob/main/Wykresy/QQploty.png) |
| ---------------------------- | ---------------------------- |
| ![Wykres_gestosci.png](https://github.com/Piterson25/Modelowanie_Projekt/blob/main/Wykresy/Wykres_gestosci.png) | ![11B-estymatory.png](https://github.com/Piterson25/Modelowanie_Projekt/blob/main/Wykresy/11B-estymatory.png) |

Celem projektu było zbadanie wzajemnych zależności między cenami akcji dwóch spółek: CD Projekt Red oraz 11 bit studios. W tym celu przeprowadzono analizę danych dotyczących cen zamknięcia tych spółek, analizę łącznego rozkładu logarytmicznych zwrotów oraz Regresji liniowej dla logarytmicznych zwrotów.

## 🛠️ Użyte technologie
Projekt został wykonany w języku R przy użyciu środowiska RStudio. W analizie danych wykorzystano następujące biblioteki:

* ggplot2
* moments
* fitdistrplus
* MASS
* survival
* ggExtra
* mnormt
* QRM
* evir

## 🚀 Uruchomienie
Aby uruchomić projekt, należy otworzyć plik R znajdujący się w folderze [Projekt](https://github.com/Piterson25/Modelowanie_Projekt/blob/main/Projekt) i uruchomić kod.

## 🎉 Funkcjonalności
### Analiza cen zamknięcia akcji
Projekt umożliwia analizę zmian cen zamknięcia akcji CD Projekt Red i 11 bit studios w czasie oraz w ujęciu zbiorczym. Dzięki wykresom można w łatwy sposób porównać zmienność cen obu spółek.

### Łączny rozkład logarytmicznych zwrotów
Projekt umożliwia analizę łącznego rozkładu logarytmicznych zwrotów dla obu spółek. Dzięki wykresom i tabelom można określić, jakie były tendencje w przeszłości oraz jakie są szanse na przyszłe zyski.

### Regresja liniowa dla logarytmicznych zwrotów
Projekt umożliwia przeprowadzenie regresji liniowej dla logarytmicznych zwrotów. Dzięki temu można określić, czy istnieje zależność między zwrotami a czasem oraz jak mocna jest ta zależność.

## Licencja
Projekt Modelowanie jest objęty licencją MIT. Więcej informacji można znaleźć w pliku [LICENSE](https://github.com/Piterson25/Modelowanie_Projekt/blob/main/LICENSE).
