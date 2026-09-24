# Globální analýza teplot oceánů a meteorologických ukazatelů

Explorační a prostorová analýza povrchových teplot moří, dynamiky větru a relativní vlhkosti vzduchu na základě globálních pobřežních měření.

---

## Příprava dat a prostředí

```r
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(readr)
library(tidyr)

# Vyhledání datového souboru v pracovním adresáři
cesta_mereni <- list.files(pattern = "teplot.*\\.csv|mori.*\\.csv|sea.*\\.csv", full.names = TRUE)[1]
if (is.na(cesta_mereni)) cesta_mereni <- "ocean_weather.csv"

# Načtení datové sady
data_ocean <- read_csv(cesta_mereni, show_col_types = FALSE)
glimpse(data_ocean)
```

---

## Distribuce fyzikálních veličin

Explorace rozdělení povrchové teploty vody, rychlosti větru a vlhkosti:

```r
# Převod vybraných proměnných do dlouhého formátu pro společné zobrazení
data_long <- data_ocean %>%
  select(matches("teplota|temp"), matches("vitr|wind"), matches("vlhkost|humid")) %>%
  pivot_longer(cols = everything(), names_to = "ukazatel", values_to = "hodnota")

# Zobrazení rozdělení veličin pomocí histogramů
ggplot(data_long, aes(x = hodnota, fill = ukazatel)) +
  geom_histogram(bins = 25, color = "white", alpha = 0.8) +
  facet_wrap(~ukazatel, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribuce sledovaných meteorologických a oceánských veličin", x = "Hodnota", y = "Četnost")
```

---

## Kontinentální komparace ukazatelů

Průměrné hodnoty sledovaných indikátorů dle kontinentů:

```r
# Výpočet průměrných hodnot veličin podle kontinentů
kontinentalni_souhrn <- data_ocean %>%
  group_by(kontinent) %>%
  summarise(
    prumer_teplota = mean(teplota_more, na.rm = TRUE),
    prumer_vitr = mean(rychlost_vetru, na.rm = TRUE),
    prumer_vlhkost = mean(vlhkost_vzduchu, na.rm = TRUE),
    .groups = "drop"
  )

print(kontinentalni_souhrn)
```

Distribuce teploty vody napříč kontinenty:

```r
# Porovnání rozdělení teploty vody pomocí boxplotů
ggplot(data_ocean, aes(x = kontinent, y = teplota_more, fill = kontinent)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Srovnání teploty moře podle kontinentů", x = "Kontinent", y = "Teplota vody (°C)")
```

---

## Globální prostorová syntéza a kartografické modely

Agregace průměrných hodnot na úrovni států a prostorové mapování:

```r
# Výpočet průměrů za jednotlivé země dle ISO kódu
staty_agregace <- data_ocean %>%
  group_by(iso_a3) %>%
  summarise(
    teplota = mean(teplota_more, na.rm = TRUE),
    vitr = mean(rychlost_vetru, na.rm = TRUE),
    vlhkost = mean(vlhkost_vzduchu, na.rm = TRUE),
    .groups = "drop"
  )

# Načtení polygonů světa a propojení s průměrnými hodnotami
mapa_sveta <- ne_countries(scale = "small", returnclass = "sf")
svovy_model <- left_join(mapa_sveta, staty_agregace, by = "iso_a3")
```

### Globální rozložení teploty vody

```r
# Choropletní mapa povrchové teploty vody
ggplot(data = svovy_model, aes(fill = teplota)) +
  geom_sf(color = "grey70", linewidth = 0.1) +
  scale_fill_viridis_c(na.value = "grey90", name = "°C") +
  labs(title = "Průměrná povrchová teplota moře") +
  theme_minimal()
```

### Globální rychlost větru

```r
# Choropletní mapa průměrné rychlosti větru
ggplot(data = svovy_model, aes(fill = vitr)) +
  geom_sf(color = "grey70", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "km/h") +
  labs(title = "Průměrná rychlost větru v pobřežních zónách") +
  theme_minimal()
```

### Relativní vlhkost vzduchu

```r
# Choropletní mapa průměrné relativní vlhkosti vzduchu
ggplot(data = svovy_model, aes(fill = vlhkost)) +
  geom_sf(color = "grey70", linewidth = 0.1) +
  scale_fill_viridis_c(option = "mako", na.value = "grey90", name = "%") +
  labs(title = "Průměrná relativní vlhkost vzduchu") +
  theme_minimal()
```
