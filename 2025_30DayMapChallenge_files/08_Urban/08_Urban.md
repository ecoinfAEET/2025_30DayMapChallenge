30 Day Map Challenge - 08_Urban
================
Elena Quintero
2025-11-08

``` r
library(tidyverse)
library(sf)
library(lwgeom)
library(rnaturalearth)
library(rnaturalearthdata)
library(paletteer)
library(showtext)
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()
```

## Read data

Read summarized data by country.

source: <https://datacatalog.worldbank.org/search/dataset/0039597>

``` r
dt_waste <- readr::read_csv("/Users/elenaqb/Downloads/maps/waste_world.csv")|> 
  mutate(waste_per_capita = total_waste / population)
```

    ## Rows: 217 Columns: 18
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): iso_a3, region_id, country, income_id, country_name, continent
    ## dbl (12): gdp, population, total_waste, composition_food_organic_waste_perce...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dt_waste <- readr::read_csv("waste_world.csv")|> 
  mutate(waste_per_capita = total_waste / population)
```

    ## Rows: 217 Columns: 18
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): iso_a3, region_id, country, income_id, country_name, continent
    ## dbl (12): gdp, population, total_waste, composition_food_organic_waste_perce...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Join with world map and reproject to Robinson

``` r
world <- ne_countries(returnclass = "sf") |> 
  left_join(dt_waste, by = c("iso_a3_eh" = "iso_a3")) |> 
  st_transform(crs = "+proj=robin")
```

Plot waste per capita

``` r
world |> 
  # filter(as.numeric(area_m2) > 3e+10) |> 
  ggplot() + 
  geom_sf(aes(fill = waste_per_capita), color = "grey40", linewidth = 0.1) +
  coord_sf(crs = "+proj=robin") + #robinson
  scale_fill_stepsn(colours = paletteer_d("PNWColors::Shuksan", direction = -1)) +
  theme_minimal(base_family = "roboto_condensed") +
  theme(axis.text = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
        legend.position = "bottom", 
        legend.title.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 30),
        legend.text = element_text(size = 23)) + 
  labs(fill = "tonnes/person/year",
       title = "Waste generation per capita",
       caption = "Data from World Bank")
```

![](08_Urban_files/figure-gfm/plot-1.png)<!-- -->

``` r
ggsave("08_Urban.png", width = 8, height = 5)
```
