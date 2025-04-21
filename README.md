# 🧬 Optymalizacja portfela inwestycyjnego z użyciem algorytmu genetycznego

## 📌 Co robi mój kod?

1. **Pobiera dane historyczne** (dzienne ceny akcji),
2. **Oblicza stopy zwrotu** z tych cen,
3. **Definiuje funkcję celu**: Sharpe Ratio (czyli: zwrot / ryzyko),
4. **Uruchamia algorytm genetyczny**, który:
   - generuje wiele losowych portfeli (czyli kombinacji wag),
   - ocenia ich Sharpe ratio,
   - krzyżuje i mutuje najlepsze rozwiązania,
   - w kolejnych iteracjach dąży do coraz lepszych rozwiązań,
5. **Zwraca portfel o najwyższym rocznym Sharpe Ratio** (czyli najlepszy kompromis między zyskiem a ryzykiem),
6. **Porównuje go z portfelem równomiernym** oraz prezentuje go **na tle 500 losowych portfeli** na wykresie ryzyko–zwrot.

---

## 📈 Sens i zastosowanie

Nasz model pokazuje, jak – na podstawie **przeszłych wyników** – można dobrać udziały poszczególnych aktywów, aby portfel był jak najbardziej opłacalny względem ponoszonego ryzyka.

> ℹ️ Jest to **optymalizacja oparta na danych historycznych** — nie gwarantuje, że w przyszłości portfel będzie równie dobry. Można ją traktować jako **symulację lub narzędzie wspomagające decyzję**.  
> W praktyce inwestycyjnej zwykle stosuje się **dodatkowe ograniczenia, aktualizacje i rebalansowanie**.

---

## ⚙️ Parametry zastosowane w projekcie

- **Okres danych:** `2021-01-01` do `2024-01-01` (3 lata danych dziennych)  
  Pokrywa cykl wzrostów i spadków, inflację, zmiany stóp procentowych.

- **`cel = 15`**  
  Zapewnia dużą precyzję kodowania wag portfela (rozdzielczość ≈ 0.00003).

- **`pop_size = 50`**  
  Równowaga między różnorodnością populacji a szybkością działania algorytmu.

- **`max_iter = 200`**  
  Pozwala na konwergencję rozwiązania w rozsądnym czasie.

- **`p_mut = 0.05`**  
  Niska mutacja — pozwala uniknąć lokalnych minimów bez destabilizacji populacji.

---

## 🧪 Wyniki

> Na podstawie danych historycznych dla czterech spółek *(Apple, Microsoft, Google, Amazon)* został zoptymalizowany skład portfela inwestycyjnego przy użyciu algorytmu genetycznego.

- **Sharpe Ratio portfela optymalnego** było **1.32× większe** niż równomiernego,
- **Microsoft (85%)** został wybrany jako najefektywniejszy składnik portfela,
- Wynik pokazuje potencjał zastosowania algorytmów heurystycznych w praktyce finansowej.

---

## 🛠️ Kluczowe poprawki w kodzie algorytmu genetycznego

### 1. 🔄 Inicjalizacja populacji

#### ⛔ STARE (błędne):
```r
population <- matrix(runif(pop_size*n*cel) <= 0, 
                     nrow = pop_size, ncol = n*cel)
#### ✅ NOWE (działające):
population <- matrix(sample(c(0,1), pop_size * n * cel, replace = TRUE),
                     nrow = pop_size, ncol = n * cel)
Dlaczego?
Poprzednia wersja generowała niemal wyłącznie FALSE, co skutkowało populacją pełną zer i brakiem różnorodności. Nowa wersja losuje prawdziwe binarne wartości (0/1) — zgodnie z zasadą działania GA.

### 2. ⚙️ Poprawa funkcji bin2int

### ⛔ STARE:
bin2int <- function(x) sum(2^(which(rev(x))-1))

### ✅ NOWE:
bin2int <- function(x) {
  x <- as.logical(x)
  idx <- which(rev(x))
  if (length(idx) == 0) return(0)
  sum(2^(idx - 1))
}

Dlaczego?
Poprzednia wersja zakładała, że x to wektor logiczny. W praktyce x pochodzi z population i może zawierać liczby (0, 1, NA), co powodowało błędy. Nowa wersja konwertuje do logicznego typu i działa stabilnie w każdej sytuacji.
