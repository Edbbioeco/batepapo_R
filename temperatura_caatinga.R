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

## Filtrar datas ----

datas <- catalogo |>
  dplyr::mutate(Ano = acquisitionDate |> lubridate::year(),
                Mes = acquisitionDate |> lubridate::month()) |>
  dplyr::group_by(Ano, Mes) |>
  dplyr::filter(tileCloudCover == 0) |>
  dplyr::slice(1) |>
  dplyr::pull(acquisitionDate)

datas

## Evalscript ----

evalscript <- "
//VERSION=3
function setup() {
  return {
    input: [\"LST\", \"dataMask\"],
    output: { bands: 2, sampleType: \"FLOAT32\" }
  }
}
function evaluatePixel(sample) {
  return [sample.LST, sample.dataMask]
}
"

evalscript

## Baixar rasters ----

dir.create("./temp")

purrr::map(datas,
           purrr::in_parallel(

             ~CDSE::GetImage(bbox = caa |> sf::st_bbox(),
                             script = evalscript,
                             time_range = .x,
                             file = paste0("./temp/temp_",
                                           .x,
                                           ".tif"),
                             collection = "sentinel-3-slstr-l2",
                             format = "image/tiff",
                             mosaicking_order = "leastRecent",
                             resolution = 1000,
                             mask = TRUE,
                             buffer = 100,
                             client = cliente)

           ),
           .progress = TRUE)

## Importar os rasters ----

rasters <- purrr::map(list.files(path = "./temp",
                                 pattern = "*.tif$",
                                 full.names = TRUE),
                      terra::rast,
                      .progress = TRUE) |>
  setNames(datas)

rasters

## Transformar ºK em ºC ----

rasters <- purrr::map(rasters,
                      ~.x - 273.15,
                      .progress = TRUE)

rasters
