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

## Coleções ----

CDSE::GetCollections()

## Buscar catálogo ----

catalogo <- CDSE::SearchCatalog(aoi = caa,
                                from = "2020-01-01",
                                to = "2026-07-01",
                                collection = "sentinel-2-l2a",
                                filter = "eo:cloud_cover < 0.01",
                                client = cliente,
                                with_geometry = FALSE)

catalogo

## Pesquisar datas ----

datas <- catalogo |>
  dplyr::mutate(Ano = acquisitionDate |> lubridate::year(),
                Mes = acquisitionDate |> lubridate::month()) |>
  dplyr::group_by(Ano, Mes) |>
  dplyr::arrange(tileCloudCover) |>
  dplyr::slice_head(n = 1) |>
  dplyr::pull(acquisitionDate)

datas

## Evalscript ----

evalscript <- rsi::spectral_indices() |>
  dplyr::filter(short_name == "NDVI") |>
  CDSE::MakeEvalScript(constellation = "sentinel-2") |>
  paste(collapse = "\n")

evalscript
