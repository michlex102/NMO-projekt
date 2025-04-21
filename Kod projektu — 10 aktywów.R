rm(list=ls())
setwd("C:/Users/Michał J/Desktop/SGH/NMO")
install.packages(c("quantmod", "PerformanceAnalytics"))

#Załadowanie danych
library(quantmod)

#Ustawienie seeda
set.seed(123)

tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", 
             "META", "NFLX", "TSLA", "NVDA", 
             "JPM", "UNH")  # przykładowe spółki
getSymbols(tickers, from="2021-01-01", to="2024-01-01")

prices <- do.call(merge, lapply(tickers, function(t) Cl(get(t))))
returns <- na.omit(ROC(prices, type="discrete"))

#Definicja fcji celu (Sharpe Ratio (średni zwrot / ryzyko) Zakładamy, że suma wag aktywów = 1)

portfolio_objective <- function(w) {
  w <- w / sum(w)
  port_returns <- returns %*% w
  port_mean <- mean(port_returns)
  port_sd <- sd(port_returns)
  #Przeliczenie na ujęcie roczne
  daily_sr <- port_mean / port_sd
  annual_sr <- daily_sr * sqrt(252)
  
  return(-annual_sr)  # bo optymalizujemy przez minimalizację
}

#Wywołanie GA z zajęć
source("genetic_algorithm.R")
#Dla 4 aktywów potrzebujemy 4 zmiennych (wag), każda w zakresie 0–1. Zmienna cel określa liczbę bitów na każdą zmienną.
n_assets <- ncol(returns)
result <- genetic_algorithm(
  f = portfolio_objective,
  x_min = rep(0, n_assets),
  x_max = rep(1, n_assets),
  cel = 15,
  pop_size = 100,
  p_mut = 0.05,
  max_iter = 300
)



#Wynik i wizualizacja

best_weights <- result$x_opt / sum(result$x_opt)
names(best_weights) <- colnames(returns)

print(best_weights)
print(-result$f_opt)  # Sharpe ratio (z minusa na plus)

best_iter <- which.min(result$f_hist)
cat("Najlepszy wynik osiągnięty w iteracji:", best_iter, "\n")

plot(result$f_hist, type='l', main="Historia najlepszego wyniku", ylab="Cel", xlab="Iteracja")

#Portfel równomeirny (To taki portfel, w którym każde aktywo ma taką samą wagę – czyli co gdy inwestujemy taką samą część kapitału w każdą spółkę)
equal_weights <- rep(1/n_assets, n_assets)
equal_return <- mean(returns %*% equal_weights)
equal_sd <- sd(returns %*% equal_weights)
equal_sr <- (equal_return / equal_sd) * sqrt(252)

#Porónanie równomiernego do naszego
barplot(
  c(equal_sr, -result$f_opt),
  names.arg = c("Równomierny", "GA optymalny"),
  col = c("gray", "green"),
  main = "Porównanie rocznego Sharpe Ratio"
)

#Losuję 500 innych portfeli i pokazuję jak wygląda w porównaniu do nich portfel równomeirny i optymalny
# Funkcja do przeliczenia danych portfela
get_stats <- function(weights, returns) {
  weights <- weights / sum(weights)
  port_returns <- returns %*% weights
  mean_return <- mean(port_returns) * 252  # przeliczenie na roczne
  sd_return <- sd(port_returns) * sqrt(252)
  return(c(mean_return, sd_return))
}
# Liczba portfeli do wygenerowania
n_random <- 500
risk_return <- matrix(NA, nrow = n_random, ncol = 2)

# Generujemy losowe portfele
for (i in 1:n_random) {
  w <- runif(n_assets)
  w <- w / sum(w)
  risk_return[i, ] <- get_stats(w, returns)
}

# Statystyki portfeli optymalnego i równomiernego
opt_stats <- get_stats(best_weights, returns)
eq_stats  <- get_stats(equal_weights, returns)

# Wykres
plot(risk_return[,2], risk_return[,1],
     col=rgb(0,0,0,0.2), pch=16,
     xlab = "Ryzyko (roczne sd)",
     ylab = "Zwrot (roczny)",
     main = "Wykres ryzyko–zwrot: losowe vs optymalne")

# Dodaj punkty optymalne i równomierne
points(opt_stats[2], opt_stats[1], col="green", pch=19, cex=1.5)
text(opt_stats[2], opt_stats[1], labels="Optymalny", pos=3, col="green")

points(eq_stats[2], eq_stats[1], col="blue", pch=19, cex=1.5)
text(eq_stats[2], eq_stats[1], labels="Równomierny", pos=3, col="blue")



#Sharpe ratio dla równomiernego i optymalnego
cat("Sharpe Ratio portfela równomiernego:", equal_sr, "\n")
cat("Sharpe Ratio portfela optymalnego:", print(-result$f_opt), "\n")
cat("Czyli SR dla portfela optymalnego jest", print(round(-result$f_opt/equal_sr, 2)),"razy większe od SR równomiernego", "\n")


# Słownik przyjaznych nazw
friendly_names <- c("Apple", "Microsoft", "Google", "Amazon", 
                    "Meta", "Netflix", "Tesla", "Nvidia", 
                    "JPMorgan", "UnitedHealth")
names(friendly_names) <- colnames(returns)  # przypisujemy do nazw z danych

# Podstawiamy przyjazne nazwy do wag
best_weights_named <- best_weights
names(best_weights_named) <- friendly_names[colnames(returns)]
best_weights_named <- round(best_weights_named * 100, 2)

cat("Najlepsze wagi portfela (w %):\n")
for (i in seq_along(best_weights_named)) {
  cat(sprintf("• %-12s: %5.2f%%\n", names(best_weights_named)[i], best_weights_named[i]))
}
#Porównanie wag, czyli składu naszego optymalnego portfela do rónomeirnego
barplot(equal_weights, names.arg = colnames(returns), main = "Wagi portfela równomiernego") #Rówomierny

bar_positions <- barplot(best_weights_named,
                         names.arg = names(best_weights_named),
                         main = "Wagi portfela optymalnego (GA)",
                         ylim = c(0, max(best_weights_named) + 10),
                         col = "lightblue")

# Etykiety lekko nad słupkami
text(x = bar_positions,
     y = best_weights_named + 2,  # 2 jednostki ponad szczytem słupka
     labels = paste0(best_weights_named, "%"),
     cex = 0.8,
     col = "black")

