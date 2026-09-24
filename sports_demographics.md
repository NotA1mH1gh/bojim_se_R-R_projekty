# Socio-demografická a institucionální analýza sportovního prostředí v ČR

Prostorová a organizační analýza registrovaného sportu v kontextu demografického rozložení obyvatelstva v jednotlivých krajích České republiky.

---

## Příprava dat a demografická báze

```r
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(RCzechia)

# Vyhledání cest ke zdrojovým souborům
f_demogr <- list.files(pattern = "demogr.*\\.xlsx", recursive = TRUE, full.names = TRUE)[1]
f_org    <- list.files(pattern = "organi.*\\.xlsx", recursive = TRUE, full.names = TRUE)[1]
f_sport  <- list.files(pattern = "sportov.*\\.xlsx", recursive = TRUE, full.names = TRUE)[1]

# Pomocná funkce pro sjednocení regionálních názvů
uprav_kraj <- function(x) {
  case_when(
    grepl("Praha", x) ~ "Hlavní město Praha",
    grepl("Vysočina", x) ~ "Kraj Vysočina",
    grepl("kraj$", x) ~ x,
    !is.na(x) & x != "" ~ paste0(trimws(x), " kraj"),
    TRUE ~ NA_character_
  )
}

# Načtení demografických údajů a filtrace krajských hodnot
demografie <- read_excel(f_demogr, sheet = 2, skip = 1) %>%
  rename(Kraj_kod = 1, Kraj_nazev = 2, Obyvatele = matches("2023")[1]) %>%
  filter(!is.na(Kraj_kod), Kraj_kod != "CZ000") %>%
  transmute(Kraj = uprav_kraj(Kraj_nazev), Obyvatele = as.numeric(Obyvatele))

# Načtení adresáře organizací se standardizací identifikátoru
org <- read_excel(f_org, col_types = "text") %>%
  mutate(ICO = sprintf("%08d", as.integer(ICO)), Kraj = uprav_kraj(Kraj))

# Načtení personálních kapacit sportovních subjektů
sport <- read_excel(f_sport) %>%
  transmute(
    ICO = sprintf("%08d", as.integer(ICO)),
    Pocet_sportovcu = coalesce(as.numeric(matches("sportov")[1]), 0),
    Pocet_treneru = coalesce(as.numeric(matches("tren")[1]), 0)
  )
```

---

## Regionální penetrace sportovců a kartografická syntéza

Agregace sportovců na úrovni krajů a propojení s demografií:

```r
# Sloučení subjektů a přepočet na počet obyvatel daného kraje
regionalni_sport <- org %>%
  inner_join(sport, by = "ICO") %>%
  filter(!is.na(Kraj)) %>%
  group_by(Kraj) %>%
  summarise(celkem_sportovcu = sum(Pocet_sportovcu, na.rm = TRUE), .groups = "drop") %>%
  inner_join(demografie, by = "Kraj") %>%
  mutate(sportovci_na_obyvatele = celkem_sportovcu / Obyvatele)

print(regionalni_sport)
```

Choropletní prostorová vizualizace:

```r
# Napojení relativní intenzity na administrativní hranice krajů
mapa <- kraje("low") %>%
  mutate(Kraj = uprav_kraj(NAZ_CZNUTS3)) %>%
  left_join(regionalni_sport, by = "Kraj")

# Vykreslení choropletní mapy ČR
ggplot(data = mapa) +
  geom_sf(aes(fill = sportovci_na_obyvatele), color = "white", linewidth = 0.4) +
  scale_fill_viridis_c(option = "viridis", name = "Sportovci na\nobyvatele") +
  labs(title = "Relativní intenzita registrovaných sportovců v krajích ČR") +
  theme_void()
```

---

## Institucionální formy a personální struktura

Filtrování verifikovaných organizací:

```r
# Výběr pouze ověřených subjektů s doplněním personálních stavů
verifikovane <- org %>%
  filter(matches("Stav")[1] == "Ověřeno") %>%
  left_join(sport, by = "ICO")
```

Dominantní právní formy a jejich regionální distribuce:

```r
# Identifikace nejfrekventovanějších právních forem
top_formy <- verifikovane %>%
  count(`Právní forma`, sort = TRUE) %>%
  slice_head(n = 4)

print(top_formy)

# Vyhodnocení pořadí právních forem v jednotlivých krajích
poradi_kraje <- verifikovane %>%
  filter(!is.na(Kraj), `Právní forma` %in% top_formy$`Právní forma`) %>%
  count(Kraj, `Právní forma`) %>%
  group_by(Kraj) %>%
  mutate(poradi = min_rank(desc(n))) %>%
  ungroup()

print(poradi_kraje)
```

Personální asymetrie u dominantní formy (Spolek):

```r
# Výpočet podílu spolků s jednostranným personálním obsazením
spolky <- verifikovane %>% filter(`Právní forma` == "Spolek")

cat("Podíl spolků s >=1 sportovcem a 0 trenéry:", 
    round(mean(spolky$Pocet_sportovcu >= 1 & spolky$Pocet_treneru == 0, na.rm = TRUE) * 100, 2), "%\n")

cat("Podíl spolků s >=1 trenérem a 0 sportovci:", 
    round(mean(spolky$Pocet_treneru >= 1 & spolky$Pocet_sportovcu == 0, na.rm = TRUE) * 100, 2), "%\n")
```

Výskyt specializovaných šachových a bridžových klubů:

```r
# Detekce specializovaných klubů podle klíčových slov v názvu
myslove_sporty <- verifikovane %>%
  filter(!is.na(Kraj), grepl("šach|sach|bridž|bridz", Název, ignore.case = TRUE)) %>%
  count(Kraj, name = "pocet_klubu", sort = TRUE)

print(myslove_sporty)

# Sloupcový graf zastoupení klubů v jednotlivých krajích
ggplot(myslove_sporty, aes(x = reorder(Kraj, pocet_klubu), y = pocet_klubu)) +
  geom_col(fill = "#7570b3") +
  coord_flip() +
  labs(title = "Zastoupení šachových a bridžových klubů dle krajů", x = "Kraj", y = "Počet klubů") +
  theme_minimal()
```
