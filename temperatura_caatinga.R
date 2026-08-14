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

# Baixar dados pelo CDSE ----

## Iniciar cliente ----

cliente <- CDSE::GetOAuthClient(id = Sys.getenv("CDSE_id"),
                                secret = Sys.getenv("CDSE_secret"))

## Buscar catálogo ----

catalogo <- CDSE::SearchCatalog(aoi = caa,
                                from = "2000-01-01",
                                to = "2026-07-01",
                                collection = "sentinel-3-slstr-l2",
                                filter = "eo:cloud_cover < 1",
                                client = cliente)

catalogo
