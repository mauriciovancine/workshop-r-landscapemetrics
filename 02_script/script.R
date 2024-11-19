#' ----
#' theme: introducao a metricas de paisagem no r
#' autor: mauricio vancine
#' data: 27/11/2024
#' ----

# preparar r --------------------------------------------------------------

# instalar e carregar pacotes
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(pdftools)) install.packages("pdftools")
if(!require(geobr)) install.packages("geobr")
if(!require(terra)) install.packages("terra")
if(!require(remotes)) install.packages("remotes")
if(!require(OpenLand)) remotes::install_github("reginalexavier/OpenLand")
if(!require(landscapemetrics)) install.packages("landscapemetrics")
# if(require(tmap)) remove.packages("r-tmap/tmap")
if(!require(tmap)) install_github("r-tmap/tmap")

# verificar tmap
packageVersion("tmap") # ‘3.99.9002’

# options
options(timeout = 600)

# importar dados ----------------------------------------------------------

## download mapbiomas ----
# download.file(url = "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_1985.tif",
#               destfile = "03_data/brasil_coverage_1985.tif", mode = "wb", )
# 
# download.file(url = "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2023.tif",
#               destfile = "03_data/brasil_coverage_2023.tif", mode = "wb")
# 
# download.file(url = "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/10/Legenda-Colecao-9-LEGEND-CODE_v2.pdf",
#               destfile = "03_data/Legenda-Colecao-9-LEGEND-CODE_v2.pdf", mode = "wb")

## importe a legenda ----
mapbiomas_legend <- pdftools::pdf_text("03_data/Legenda-Colecao-9-LEGEND-CODE_v2.pdf") %>% 
    stringr::str_split("[\\r\\n]+") %>% 
    unlist() %>% 
    stringr::str_trim() %>% 
    stringr::str_split_fixed("\\s{2,}", 5) %>% 
    tibble::as_tibble() %>% 
    dplyr::select(1, 3, 4) %>% 
    dplyr::slice(-c(1:5, 43)) %>% 
    dplyr::rename(class = 1, value = 2, color = 3) %>% 
    dplyr::mutate(value = as.numeric(value),
                  class = str_trim(str_replace_all(class, "[0-9\\.]", ""))) %>% 
    dplyr::bind_rows(tibble::tibble(class = "Não definido", value = 0, color = "gray"))
mapbiomas_legend

## limite santa maria ----
santa_maria <- geobr::read_municipality(code_muni = 4316907, year = 2020) %>% 
    sf::st_transform(4326) %>% 
    terra::vect()
santa_maria

## importar
santa_maria <- terra::vect("03_data/santa_maria.shp")
santa_maria

## importar ----
mapbiomas1985_santa_maria <- terra::rast("03_data/mapbiomas1985_santa_maria.tif") %>% 
    terra::crop(santa_maria, mask = TRUE)
mapbiomas1985_santa_maria

mapbiomas2023_santa_maria <- terra::rast("03_data/mapbiomas2023_santa_maria.tif") %>% 
    terra::crop(santa_maria, mask = TRUE)
mapbiomas2023_santa_maria

## classes
mapbiomas1985_santa_maria_classes <- terra::freq(mapbiomas1985_santa_maria) %>% 
    dplyr::left_join(mapbiomas_legend) %>% 
    dplyr::arrange(value)
mapbiomas1985_santa_maria_classes

mapbiomas2023_santa_maria_classes <- terra::freq(mapbiomas2023_santa_maria) %>% 
    dplyr::left_join(mapbiomas_legend) %>% 
    dplyr::arrange(value)
mapbiomas2023_santa_maria_classes

# plot - vai demorar um pouco...
tm_shape(mapbiomas1985_santa_maria) +
    tm_raster(col = "brasil_coverage_1985", 
              col.scale = tm_scale_categorical(values = mapbiomas1985_santa_maria_classes$color,
                                               labels = paste0(mapbiomas1985_santa_maria_classes$value, "-", mapbiomas1985_santa_maria_classes$class)),
              col.legend = tm_legend(title = "Uso e cobertura 1985",
                                     position = tm_pos_out("right", "center")))

tm_shape(mapbiomas2023_santa_maria) +
    tm_raster(col = "brasil_coverage_2023", 
              col.scale = tm_scale_categorical(values = mapbiomas2023_santa_maria_classes$color,
                                               labels = paste0(mapbiomas2023_santa_maria_classes$value, "-", mapbiomas2023_santa_maria_classes$class)),
              col.legend = tm_legend(title = "Uso e cobertura 2023",
                                     position = tm_pos_out("right", "center")))

# checar o raster --------------------------------------------------------

## checar o raster ----
landscapemetrics::check_landscape(mapbiomas1985_santa_maria)
landscapemetrics::check_landscape(mapbiomas2023_santa_maria)

#' prerequisitos do raster
#' 1. sistema de referencias de coordenadas e projetada (crs)
#' 2. unidade esta em metros (units)
#' 3. classes como valores inteiros (class)
#' 4. numero de classes (n_class)

## reprojetar ----
mapbiomas1985_santa_maria_utm <- terra::project(mapbiomas1985_santa_maria, "EPSG:32722", method = "near")
mapbiomas1985_santa_maria_utm

mapbiomas2023_santa_maria_utm <- terra::project(mapbiomas2023_santa_maria, "EPSG:32722", method = "near")
mapbiomas2023_santa_maria_utm

## checar novamente o raster ----
landscapemetrics::check_landscape(mapbiomas1985_santa_maria_utm)
landscapemetrics::check_landscape(mapbiomas2023_santa_maria_utm)

# listar as metricas ------------------------------------------------------

## metricas ----
all_metrics <- landscapemetrics::list_lsm()
all_metrics

## patch metrics ----
patch_metrics <- landscapemetrics::list_lsm() %>%
    dplyr::filter(level == "patch") %>% 
    dplyr::arrange(type)
patch_metrics

patch_metrics %>%
    dplyr::group_by(type) %>% 
    dplyr::summarise(n = n())

# class metrics
class_metrics <- landscapemetrics::list_lsm() %>%
    dplyr::filter(level == "class") %>% 
    dplyr::arrange(type)
class_metrics

class_metrics_type <- class_metrics %>%
    dplyr::group_by(type) %>% 
    dplyr::summarise(n = n())
class_metrics_type

# landscape metrics
landscape_metrics <- landscapemetrics::list_lsm() %>%
    dplyr::filter(level == "landscape") %>% 
    dplyr::arrange(type)
landscape_metrics

landscape_metrics_type <- landscape_metrics %>%
    dplyr::group_by(type) %>% 
    dplyr::summarise(n = n())
landscape_metrics_type

# mapas -------------------------------------------------------------------

# plotar paisagem e metricas
landscapemetrics::show_patches(landscape = mapbiomas1985_santa_maria_utm, class = 12, directions = 8)
landscapemetrics::show_patches(landscape = mapbiomas1985_santa_maria_utm, class = 12, directions = 4)

landscapemetrics::show_cores(landscape = paisagens_list[[1]], 
                             class = 4, edge_depth = 1)

landscapemetrics::show_cores(landscape = paisagens_list[[1]], 
                             class = 4, edge_depth = 2)

landscapemetrics::show_lsm(landscape = paisagens_list[[1]], 
                           what = "lsm_p_area", class = 4)

# calcular as metricas ----------------------------------------------------

#' estrutura das funcoes
#' 1. prefixo: ‘lsm_’
#' 2. nivel: ‘p’, ‘c’ e ‘l’ para patch, class e landscape level
#' 3. metrica: patch area - ‘lsm_p_area’
#' 4. todas as funcoes funcionam para rasterlayers, rasterstack/rasterbrick ou list
#' 5. algumas funcoes permitem add parametros: edge depth ou cell neighbourhood rule

# area no nivel de mancha (patch - p)
paisagens_area_p <- landscapemetrics::lsm_p_area(landscape = paisagens_list[[1]])
paisagens_area_p

paisagens_area_p_all <- landscapemetrics::lsm_p_area(landscape = paisagens_list)
paisagens_area_p_all

# area no nivel de classe (class - c)
paisagens_area_c <- landscapemetrics::lsm_c_area_mn(landscape = paisagens_list[[1]])
paisagens_area_c

paisagens_area_c_all <- landscapemetrics::lsm_c_area_mn(landscape = paisagens_list)
paisagens_area_c_all

# area no nivel de paisagem (landscape - l)
paisagens_area_l <- landscapemetrics::lsm_l_area_mn(landscape = paisagens_list[[1]])
paisagens_area_l

paisagens_area_l_all <- landscapemetrics::lsm_l_area_mn(landscape = paisagens_list)
paisagens_area_l_all

# calcular todas as metricas por nivel ------------------------------------

#' calculate_lsm()
#' calcula varias metricas simultaneamente
#' facilita a entrada de parametros
#' permite escolha por ‘level’, ‘metric’, ‘name’, ‘type’, ‘what’

# patch level
lsm_patch <- landscapemetrics::calculate_lsm(landscape = paisagens_list[[1]], 
                                             level = "patch", 
                                             edge_depth = 1, # celulas
                                             neighbourhood = 8, # oito celulas nas vizinhancas
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_patch

# class level
lsm_class <- landscapemetrics::calculate_lsm(landscape = paisagens_list[[1]], 
                                             level = "class", 
                                             edge_depth = 1, # celulas
                                             neighbourhood = 8, # oito celulas nas vizinhancas
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_class

# landscape level
lsm_landscape <- landscapemetrics::calculate_lsm(landscape = paisagens_list[[1]], 
                                                 level = "landscape",
                                                 edge_depth = 1, # celulas
                                                 neighbourhood = 8, # oito celulas nas vizinhancas
                                                 full_name = TRUE, 
                                                 verbose = TRUE, 
                                                 progress = TRUE)
lsm_landscape

# espacializar as metricas ------------------------------------

# reclassificar
paisagen01_forest <- raster::reclassify(x = paisagens_list[[1]], rcl = c(0,3,NA, 3,4,1))
paisagen01_forest

tm_shape(paisagen01_forest) +
    tm_raster(pal = "forestgreen", legend.show = FALSE)

# calcular e espacializar
paisagen01_forest_patch <- landscapemetrics::spatialize_lsm(paisagen01_forest,
                                                            what = "patch", 
                                                            progress = TRUE)
paisagen01_forest_patch

# mapa
tm_shape(paisagen01_forest_patch$layer_1$lsm_p_area) +
    tm_raster(pal = "viridis", title = "Área (ha)")

tm_shape(paisagen01_forest_patch$layer_1$lsm_p_shape) +
    tm_raster(pal = "viridis", title = "Formato")

tm_shape(paisagen01_forest_patch$layer_1$lsm_p_enn) +
    tm_raster(pal = "viridis", title = "Distância (m)")

# multiplas escalas -------------------------------------------------------

# tamanhos
tamanhos <- c(100, 200, 500, 1000)
tamanhos

# multiplos buffers
buffers_multi <- sf::st_buffer(x = sf::st_as_sf(purrr::map_dfr(amost_vetor, rep, 4)), 
                               dist = rep(tamanhos, each = 10))
buffers_multi$buffer <- rep(tamanhos, each = 10)
buffers_multi

# map
tm_shape(uso_raster) +
    tm_raster(style = "cat", title = "Legenda", alpha = .5,
              palette = c("blue", "orange", "gray", "forestgreen", "green")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red") 

# metricas multiplas escalas
metricas_multiplas_escalas <- tamanhos %>% 
    set_names() %>% 
    map_dfr(~sample_lsm(landscape = uso_raster, 
                        y = amost_vetor, 
                        shape = "circle",
                        size = .,
                        what = c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                        all_classes = TRUE,
                        return_raster = TRUE,
                        verbose = TRUE,
                        progress = TRUE), 
            .id = "buffer")
metricas_multiplas_escalas

# map
tm_shape(paisagens_list[[1]]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red") +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

tm_shape(metricas_multiplas_escalas[metricas_multiplas_escalas$layer == 1 &
                                        metricas_multiplas_escalas$buffer == 100, ]$raster_sample_plots[[1]], 
         bbox = buffers_multi[31,]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red")  +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

tm_shape(metricas_multiplas_escalas[metricas_multiplas_escalas$layer == 1 &
                                        metricas_multiplas_escalas$buffer == 200, ]$raster_sample_plots[[1]], 
         bbox = buffers_multi[31,]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red")  +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

tm_shape(metricas_multiplas_escalas[metricas_multiplas_escalas$layer == 1 &
                                        metricas_multiplas_escalas$buffer == 500, ]$raster_sample_plots[[1]], 
         bbox = buffers_multi[31,]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red")  +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

tm_shape(metricas_multiplas_escalas[metricas_multiplas_escalas$layer == 1 &
                                        metricas_multiplas_escalas$buffer == 1000, ]$raster_sample_plots[[1]], 
         bbox = buffers_multi[31,]) +
    tm_raster(style = "cat", title = "Legenda", alpha = .7,
              palette = c("blue", "orange", "forestgreen")) +
    tm_shape(buffers_multi) +
    tm_borders(col = "red")  +
    tm_shape(amost_vetor[1, ]) +
    tm_bubbles()

# end ---------------------------------------------------------------------