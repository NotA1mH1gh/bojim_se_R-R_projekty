# Kvantitativní analýza úspěšnosti crowdfundingových projektů

Komplexní explorace a statistické modelování faktorů ovlivňujících úspěšnost financování projektů s využitím finančních ukazatelů, zapojení podporovatelů a sektorové kategorizace.

---

## Příprava a načtení dat

```r
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(scales)

# Načtení relačních datových souborů
df_projekty   <- read_csv("data/projects.csv", show_col_types = FALSE)
df_kategorie  <- read_csv("data/categories.csv", show_col_types = FALSE)
df_zeme       <- read_csv("data/countries.csv", show_col_types = FALSE)
df_status     <- read_csv("data/project_status.csv", show_col_types = FALSE)
df_finance    <- read_csv("data/project_finance.csv", show_col_types = FALSE)
df_cas        <- read_csv("data/project_dates.csv", show_col_types = FALSE)
```

---

## Sektorová a geografická struktura projektů

Konsolidace projektových dat:

```r
# Sloučení metadatových tabulek do jednotné databáze
projekty_komplet <- df_projekty %>%
  inner_join(df_kategorie, by = "category_id") %>%
  inner_join(df_zeme, by = "country_id") %>%
  inner_join(df_status, by = "project_id")
```

Zastoupení hlavních sektorů:

```r
# Výpočet četností projektů podle odvětví
sektory_prehled <- projekty_komplet %>%
  count(main_category, name = "pocet_projektu", sort = TRUE)

print(sektory_prehled)

# Sloupcový graf rozdělení projektů napříč kategoriemi
ggplot(sektory_prehled, aes(x = reorder(main_category, pocet_projektu), y = pocet_projektu)) +
  geom_col(fill = "#2c7bb6") +
  coord_flip() +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(title = "Distribuce projektů dle hlavních odvětví", x = "Odvětví", y = "Počet") +
  theme_minimal()
```

Míra úspěšnosti v jednotlivých odvětvích:

```r
# Podíl úspěšných projektů u ukončených kampaní
uspesnost_odvetvi <- projekty_komplet %>%
  filter(state != "live") %>%
  group_by(main_category) %>%
  summarise(
    celkem = n(),
    uspesne = sum(state == "successful"),
    mira_uspechu = uspesne / celkem,
    .groups = "drop"
  ) %>%
  arrange(desc(mira_uspechu))

print(uspesnost_odvetvi)
```

---

## Finanční profil a chování komunity

Analýza dokončených kampaní:

```r
# Výpočet poměru vybraných financí k cíli u uzavřených projektů
projekty_finance <- projekty_komplet %>%
  inner_join(df_finance, by = "project_id") %>%
  filter(state %in% c("successful", "failed")) %>%
  mutate(funding_ratio = usd_pledged / usd_goal)

# Souhrnné statistiky finančních metrik
souhrn_financi <- projekty_finance %>%
  group_by(state) %>%
  summarise(
    pocet = n(),
    median_cil = median(usd_goal),
    median_vybrano = median(usd_pledged),
    median_podporovatelu = median(backers_count),
    median_funding_ratio = median(funding_ratio),
    .groups = "drop"
  )

print(souhrn_financi)
```

Srovnání rozpočtových cílů:

```r
# Porovnání cílových částek na logaritmické ose
ggplot(projekty_finance, aes(x = state, y = usd_goal, fill = state)) +
  geom_boxplot() +
  scale_y_log10(labels = label_dollar()) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Rozpočtové cíle úspěšných a neúspěšných kampaní", x = "Výsledek", y = "Cíl (USD, log)")
```

Vztah mezi rozpočtem a počtem přispěvatelů:

```r
# Bodový graf závislosti rozpočtu na podpoře komunity
ggplot(projekty_finance, aes(x = usd_goal, y = backers_count, color = state)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(labels = label_dollar()) +
  scale_y_log10(labels = label_number(big.mark = " ")) +
  theme_minimal() +
  labs(title = "Finanční cíl vs. velikost podporovatelské základny", x = "Cíl (USD, log)", y = "Podporovatelé (log)")
```

---

## Dlouhodobý trend a statistické testování asociace

Časová trajektorie nových projektů:

```r
# Extrakce roku spuštění kampaně
projekty_trendy <- projekty_finance %>%
  inner_join(df_cas, by = "project_id") %>%
  mutate(rok = year(as_date(launched_at)))

# Roční a kumulativní přírůstky projektů
trendy_roky <- projekty_trendy %>%
  count(rok, name = "pocet_v_roce") %>%
  arrange(rok) %>%
  mutate(kumulativni_pocet = cumsum(pocet_v_roce))

print(trendy_roky)

# Křivka celkového kumulativního růstu
ggplot(trendy_roky, aes(x = factor(rok), y = kumulativni_pocet, group = 1)) +
  geom_line(color = "#386cb0", linewidth = 1.2) +
  geom_point(color = "#386cb0", size = 3) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  theme_minimal() +
  labs(title = "Kumulativní růst objemu projektů v čase", x = "Rok", y = "Kumulativní počet")
```

Kumulativní nárůst pro klíčová odvětví:

```r
# Výběr dominantních odvětví a výpočet dílčích kumulativních součtů
top_odvetvi <- sektory_prehled %>% slice_head(n = 4) %>% pull(main_category)

trendy_odvetvi <- projekty_trendy %>%
  filter(main_category %in% top_odvetvi) %>%
  count(main_category, rok, name = "pocet_v_roce") %>%
  arrange(main_category, rok) %>%
  group_by(main_category) %>%
  mutate(kumulativni_pocet = cumsum(pocet_v_roce)) %>%
  ungroup()

# Kumulativní křivky pro jednotlivá hlavní odvětví
ggplot(trendy_odvetvi, aes(x = factor(rok), y = kumulativni_pocet, color = main_category, group = main_category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  theme_minimal() +
  labs(title = "Kumulativní trajektorie klíčových sektorů", x = "Rok", y = "Kumulativní počet", color = "Odvětví")
```

Statistický test nezávislosti výsledku na odvětví:

```r
# Kontingenční tabulka četností kategorií vůči výsledku
kontingencni_matice <- table(projekty_trendy$main_category, projekty_trendy$state)
print(kontingencni_matice)

# Chí-kvadrát test nezávislosti
vysledek_testu <- chisq.test(kontingencni_matice)
print(vysledek_testu)
```
