# Pacotes ----

library(geobr)

library(tidyverse)

library(CDSE)

library(rsi)

library(terra)

library(tidyterra)

library(ggview)

# Shapefile da caatinga ----

## Baixar ----

caa <- geobr::read_biomes(year = 2025) |>
  dplyr::filter(name_biome == "Caatinga")

## Visualizar ----

caa

ggplot() +
  geom_sf(data = caa)

