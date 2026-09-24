# Analýza a synchronizace preskripčních dat léčiv v ČR

Analytická studie zaměřená na audit, párování a časovou dynamiku preskripce specializovaných léčiv ve zdravotnických zařízeních s využitím mezinárodní klasifikace WHO ATC a národních identifikátorů SÚKL.

---

## Načtení dat a konfigurace

```r
library(readxl)
library(readr)
library(writexl)
library(dplyr)
library(ggplot2)

# Vyhledání cest k datovým souborům
f_kos <- list.files(pattern = "koš|kos", recursive = TRUE, full.names = TRUE)[1]
f_centra <- list.files(pattern = "center|centra", recursive = TRUE, full.names = TRUE)[1]
f_atc <- list.files(pattern = "atc|sukl", recursive = TRUE, full.names = TRUE)[1]

# Načtení tabulek jako text pro zachování formátu kódů
kos <- read_excel(f_kos, col_types = "text")
centra <- read_excel(f_centra, col_types = "text")
prevodnik <- read_csv(f_atc, col_types = cols(.default = "c"))
```

---

## Synchronizace a aktualizace preskripční databáze

Párování aktuálních center s národními kódy léčiv a detekce změn:

```r
# Propojení center s národními kódy léčiv přes mezinárodní ATC
centra_par <- centra %>%
  rename(ICZ = 1, ICZ_NAME = 2, ATC = matches("ATC")[1]) %>%
  inner_join(prevodnik, by = c("ATC" = "WHO_ATC5")) %>%
  distinct(ICZ, SUKL, .keep_all = TRUE)

# Detekce ukončených a nově zavedených preskripcí
vyrazena <- kos %>% 
  filter(is.na(PERIOD_TO) | PERIOD_TO == "" | PERIOD_TO == "NA") %>% 
  anti_join(centra_par, by = c("ICZ", "SUKL"))

nova <- centra_par %>% 
  anti_join(kos, by = c("ICZ", "SUKL"))

datum <- "202503"

# Zápis data ukončení u vyřazených a doplnění nových položek
databaze_nova <- kos %>%
  mutate(PERIOD_TO = if_else(is.na(PERIOD_TO) & paste(ICZ, SUKL) %in% paste(vyrazena$ICZ, vyrazena$SUKL), datum, PERIOD_TO)) %>%
  bind_rows(nova %>% select(ICZ, ICZ_NAME, SUKL) %>% mutate(PERIOD_FROM = datum, PERIOD_TO = NA_character_))

# Export konsolidované databáze
write_xlsx(databaze_nova, "aktualizovany_registr_leciv.xlsx")
```

---

## Dlouhodobý vývoj a časová dynamika preskripce

Agregace ročních přírůstků a kumulativní součty:

```r
# Roční počty nově zařazených léčiv
prirustky <- databaze_nova %>%
  filter(!is.na(PERIOD_FROM)) %>%
  mutate(rok = substr(PERIOD_FROM, 1, 4)) %>%
  count(rok, name = "pridano")

# Roční počty vyřazených léčiv
ubytky <- databaze_nova %>%
  filter(!is.na(PERIOD_TO) & PERIOD_TO != "" & PERIOD_TO != "NA") %>%
  mutate(rok = substr(PERIOD_TO, 1, 4)) %>%
  count(rok, name = "odebrano")

# Časová osa a výpočet kumulativních ukazatelů
dynamika <- full_join(prirustky, ubytky, by = "rok") %>%
  mutate(across(everything(), ~coalesce(., 0))) %>%
  arrange(rok) %>%
  mutate(
    kumul_pridano = cumsum(pridano),
    kumul_odebrano = cumsum(odebrano),
    rozdil = pridano - odebrano
  )

print(dynamika)
```

Grafické srovnání trendů:

```r
# Vykreslení kumulativního vývoje v čase
ggplot(dynamika, aes(x = factor(rok))) +
  geom_line(aes(y = kumul_pridano, group = 1, color = "Zařazená léčiva"), linewidth = 1.2) +
  geom_line(aes(y = kumul_odebrano, group = 1, color = "Vyřazená léčiva"), linewidth = 1.2) +
  geom_point(aes(y = kumul_pridano, color = "Zařazená léčiva"), size = 2.5) +
  geom_point(aes(y = kumul_odebrano, color = "Vyřazená léčiva"), size = 2.5) +
  theme_minimal() +
  labs(title = "Kumulativní vývoj zařazených a vyřazených léčiv", x = "Rok", y = "Počet", color = "Typ")
```

Roky s největšími odchylkami:

```r
# Výběr období s nejvýraznější meziroční změnou
dynamika %>%
  arrange(desc(abs(rozdil))) %>%
  head(5)
```
