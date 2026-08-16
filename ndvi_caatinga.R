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

rec <- rec <- geobr::read_municipality(year = 2025) |>
  dplyr::filter(name_muni == "Recife")

## Visualizar ----

rec

ggplot() +
  geom_sf(data = rec)

# Baixar dados pelo CDSE ----

## Iniciar cliente ----

cliente <- CDSE::GetOAuthClient(id = Sys.getenv("CDSE_id"),
                                secret = Sys.getenv("CDSE_secret"))

## Coleções ----

CDSE::GetCollections()

## Buscar catálogo ----

catalogo <- CDSE::SearchCatalog(aoi = rec,
                                from = "2000-01-01",
                                to = "2026-07-01",
                                collection = "sentinel-2-l2a",
                                filter = "eo:cloud_cover < 25",
                                client = cliente,
                                with_geometry = FALSE)

catalogo

## Pesquisar datas ----

datas <- catalogo |>
  dplyr::mutate(mes_ref = acquisitionDate |>
                  lubridate::floor_date("month")) |>
  dplyr::distinct(mes_ref) |>
  dplyr::arrange(mes_ref) |>
  dplyr::mutate(inicio = mes_ref,
                fim = mes_ref |>
                  lubridate::ceiling_date("month") - lubridate::days(1),
                intervalo = purrr::map2(inicio,
                                        fim,
                                        ~c(as.character(.x),
                                           as.character(.y)))) |>
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

                        ~CDSE::GetImage(bbox = rec |> sf::st_bbox(),
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
                               terra::crop(rec) |>
                               terra::mask(rec)

                           ),
                           .progress = TRUE)

rasters_trat

## Visualizar rasters ----

purrr::imap(
  rasters_trat,
  purrr::in_parallel(

    ~ggplot() +
      geom_spatraster(data = .x) +
      tidyterra::scale_fill_hypso_c(palette = "colombia_hypso",
                                    direction = -1) +
      labs(title = .y)

    ),
  .progress = TRUE)

ggplot() +
  geom_spatraster(data = rasters_trat[[sample(1:length(rasters_trat),
                                              1)]]) +
  tidyterra::scale_fill_hypso_c(palette = "colombia_hypso",
                                direction = -1) +
  labs(title = rasters_trat |> names())
