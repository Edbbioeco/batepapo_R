# Pacotes ----

library(geobr)

library(tidyverse)

library(CDSE)

library(rsi)

library(terra)

library(tidyterra)

library(ggview)

library(gganimate)

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
                                collection = "landsat-ot-l1",
                                filter = "eo:cloud_cover < 50",
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
  CDSE::MakeEvalScript(constellation = "landsat") |>
  paste(collapse = "\n")

evalscript

## Baixar dados ----

rasters <- purrr::map(datas,
                      purrr::in_parallel(

                        ~CDSE::GetImage(bbox = rec |> sf::st_bbox(),
                                        time_range = .x,
                                        script = evalscript,
                                        collection = "landsat-ot-l1",
                                        format = "image/tiff",
                                        mosaicking_order = "leastCC",
                                        resolution = 50,
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
                                    direction = -1,
                                    limits = c(-1, 1)) +
      labs(title = .y)

    ),
  .progress = TRUE)

ggplot() +
  geom_spatraster(data = rasters_trat[[
    sample(1:length(rasters_trat),
           1)]]) +
  tidyterra::scale_fill_hypso_c(palette = "colombia_hypso",
                                direction = -1) +
  labs(title = rasters_trat[
    sample(1:length(rasters_trat),
           1)] |> names())

## Série temporal ----

### Extrair valores médios e desvio padrão ----

df_ndvi <- purrr::map2_dfr(
  rasters_trat,
  datas,
  purrr::in_parallel(

    ~tibble::tibble(Data = .y[[1]] |>
                      lubridate::as_date(),
                    `NDVI médio` = .x |>
                      terra::values() |>
                      na.omit() |>
                      mean(),
                    sd = .x |>
                      terra::values() |>
                      na.omit() |>
                      sd())

    ),
  .progress = TRUE)

df_ndvi

### Gráfico ----

df_ndvi |>
  ggplot(aes(Data, `NDVI médio`)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20)) +
  ggview::canvas(height = 10, width = 12)

df_ndvi |>
  ggplot(aes(Data, `NDVI médio`)) +
  geom_ribbon(aes(ymin = `NDVI médio` - sd,
                  ymax = `NDVI médio` + sd,
                  fill = "Desvio Padrão"),
              alpha = 0.3) +
  scale_fill_manual(values = c("limegreen")) +
  geom_line(color = "green4",
            linewidth = 1) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

### Gráfiocos animados ----

ndvi_animado <- df_ndvi |>
  ggplot(aes(Data, `NDVI médio`)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20)) +
  gganimate::transition_reveal(Data)

gganimate::animate(ndvi_animado,
                   width = 1280,
                   height = 1066,
                   nframes = 150,
                   fps = 20,
                   renderer = gganimate::av_renderer("./serie_temporal_recife_ndvi.mp4"))

ndvi_animado_sd <- df_ndvi |>
  ggplot(aes(Data, `NDVI médio`)) +
  geom_ribbon(aes(ymin = `NDVI médio` - sd,
                  ymax = `NDVI médio` + sd,
                  fill = "Desvio Padrão"),
              alpha = 0.3) +
  scale_fill_manual(values = c("limegreen")) +
  geom_line(color = "green4",
            linewidth = 1) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  gganimate::transition_reveal(Data)

gganimate::animate(ndvi_animado_sd,
                   width = 1280,
                   height = 1066,
                   nframes = 150,
                   fps = 20,
                   renderer = gganimate::av_renderer("./serie_temporal_recife_ndvi_sd.mp4"))
