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
  dplyr::summarise(inicio = min(acquisitionDate),
                   fim = max(acquisitionDate),
                   .groups = "drop") |>
  dplyr::mutate(intervalo = purrr::map2(inicio,
                                        fim,
                                        ~c(.x, .y)
                                        )) |>
  dplyr::pull(intervalo)

datas

## Evalscript ----

evalscript <- rsi::spectral_indices() |>
  dplyr::filter(short_name == "NDVI") |>
  CDSE::MakeEvalScript(constellation = "sentinel-2") |>
  paste(collapse = "\n")

evalscript

## Baixar dados ----

rasters <- purrr::map(datas,
                      purrr::in_parallel(

                        ~CDSE::GetImage(bbox = caa |> sf::st_bbox(),
                                        time_range = .x,
                                        script = evalscript,
                                        collection = "sentinel-2-l2a",
                                        format = "image/tiff",
                                        mosaicking_order = "leastCC",
                                        resolution = 1000,
                                        mask = FALSE,
                                        buffer = 100,
                                        client = cliente)

                      ),
                      .progress = TRUE) |>
  setNames(purrr::map(datas,
                      ~paste0(.x[1],
                              " - ",
                              .x[2])))

rasters

## Recortar rasters ----

rasters_trat <- purrr::map(rasters,
                           purrr::in_parallel(

                             ~.x |>
                               terra::crop(caa) |>
                               terra::mask(caa)

                           ),
                           .progress = TRUE)

rasters_trat

## Visualizar rasters ----

purrr::imap(
  rasters_trat,
  purrr::in_parallel(

    ~ggplot() +
      geom_spatraster(data = .x) +
      tidyterra::scale_fill_hypso_c(palette = "colombia_hypso") +
      labs(title = .y)

    ),
  .progress = TRUE)
