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
if(!require(raster)) install.packages("raster")
if(!require(terra)) install.packages("terra")
if(!require(remotes)) install.packages("remotes")
if(!require(OpenLand)) remotes::install_github("reginalexavier/OpenLand")
if(!require(networkD3)) install.packages("networkD3")
if(!require(webshot2)) install.packages("webshot2")
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
    dplyr::bind_rows(tibble::tibble(class = "Não definido", value = 0, color = "#c3c3c3"))
mapbiomas_legend

## limite santa maria ----
santa_maria <- geobr::read_municipality(code_muni = 4316907, year = 2020) %>% 
    sf::st_transform(4326) %>% 
    terra::vect()
santa_maria

## importar vetor ----
santa_maria <- terra::vect("03_data/santa_maria.shp")
santa_maria

## importar raster ----
mapbiomas_1985_santa_maria <- terra::rast("03_data/mapbiomas1985_santa_maria.tif") %>% 
    terra::crop(santa_maria, mask = TRUE)
names(mapbiomas_1985_santa_maria) <- "santamaria_1985"
mapbiomas_1985_santa_maria

mapbiomas_2023_santa_maria <- terra::rast("03_data/mapbiomas2023_santa_maria.tif") %>% 
    terra::crop(santa_maria, mask = TRUE)
names(mapbiomas_2023_santa_maria) <- "santamaria_2023"
mapbiomas_2023_santa_maria

## classes
mapbiomas_1985_santa_maria_classes <- terra::freq(mapbiomas_1985_santa_maria) %>% 
    dplyr::left_join(mapbiomas_legend) %>% 
    dplyr::arrange(value)
mapbiomas_1985_santa_maria_classes

mapbiomas_2023_santa_maria_classes <- terra::freq(mapbiomas_2023_santa_maria) %>% 
    dplyr::left_join(mapbiomas_legend) %>% 
    dplyr::arrange(value)
mapbiomas_2023_santa_maria_classes

# plot - vai demorar um pouco...
map_lulc_1985 <- tm_shape(mapbiomas_1985_santa_maria) +
    tm_raster(col = "santamaria_1985", 
              col.scale = tm_scale_categorical(values = mapbiomas_1985_santa_maria_classes$color,
                                               labels = paste0(mapbiomas_1985_santa_maria_classes$value, "-", mapbiomas_1985_santa_maria_classes$class)),
              col.legend = tm_legend(title = "Uso e cobertura 1985",
                                     position = tm_pos_out("right", "center"))) +
    tm_compass(size = 3, position = c(.9, .2)) +
    tm_scalebar(text.size = 1, breaks = c(0, 5, 10), position = c(.75, .1))
map_lulc_1985
tmap_save(map_lulc_1985, "04_results/map_1985_lulc.png", width = 30, height = 20, units = "cm", dpi = 300)

map_lulc_2023 <- tm_shape(mapbiomas_2023_santa_maria) +
    tm_raster(col = "santamaria_2023", 
              col.scale = tm_scale_categorical(values = mapbiomas_2023_santa_maria_classes$color,
                                               labels = paste0(mapbiomas_2023_santa_maria_classes$value, "-", mapbiomas_2023_santa_maria_classes$class)),
              col.legend = tm_legend(title = "Uso e cobertura 2023",
                                     position = tm_pos_out("right", "center"))) +
    tm_compass(size = 3, position = c(.9, .2)) +
    tm_scalebar(text.size = 1, breaks = c(0, 5, 10), position = c(.75, .1))
map_lulc_2023
tmap_save(map_lulc_2023, "04_results/map_2023_lulc.png", width = 30, height = 20, units = "cm", dpi = 300)

# checar o raster --------------------------------------------------------

## checar o raster ----
landscapemetrics::check_landscape(mapbiomas_1985_santa_maria)
landscapemetrics::check_landscape(mapbiomas_2023_santa_maria)

#' prerequisitos do raster
#' 1. sistema de referencias de coordenadas e projetada (crs)
#' 2. unidade esta em metros (units)
#' 3. classes como valores inteiros (class)
#' 4. numero de classes (n_class)

## reprojetar ----
mapbiomas_1985_santa_maria_utm <- terra::project(mapbiomas_1985_santa_maria, "EPSG:32722", method = "near")
mapbiomas_1985_santa_maria_utm

plot(mapbiomas_1985_santa_maria)
plot(mapbiomas_1985_santa_maria_utm)

mapbiomas_2023_santa_maria_utm <- terra::project(mapbiomas_2023_santa_maria, "EPSG:32722", method = "near")
mapbiomas_2023_santa_maria_utm

plot(mapbiomas_2023_santa_maria)
plot(mapbiomas_2023_santa_maria_utm)

## checar novamente o raster ----
landscapemetrics::check_landscape(mapbiomas_1985_santa_maria_utm)
landscapemetrics::check_landscape(mapbiomas_2023_santa_maria_utm)

## reprojetar vetor ----
santa_maria_utm <- terra::project(santa_maria, "EPSG:32722")
santa_maria_utm

plot(santa_maria, col = "gray")
plot(santa_maria_utm, col = "gray")

# diagrama de sankey ------------------------------------------------------

## preparar dados ----
mapbiomas_1985_2023_santa_maria_utm <- c(mapbiomas_1985_santa_maria_utm, mapbiomas_2023_santa_maria_utm) %>% 
    raster::stack()
mapbiomas_1985_2023_santa_maria_utm

## tabela de contingencia ----
ct <- OpenLand::contingencyTable(input_raster = mapbiomas_1985_2023_santa_maria_utm)
ct

ct$lulc_Multistep
ct$lulc_Onestep
ct$tb_legend
ct$totalArea
ct$totalInterval

# ajuste dos nomes das classes
tb_legend <- ct$tb_legend %>%
    dplyr::select(-color) %>% 
    dplyr::left_join(mapbiomas_legend, by = c("categoryValue" = "value")) %>% 
    dplyr::mutate(categoryValue = as.integer(categoryValue),
                  categoryName = as.factor(class)) %>% 
    dplyr::select(-class) %>% 
    dplyr::mutate(categoryName = c("nd", "ff", "sil", "ca", "fc", "pas", "mos", 
                                   "urb", "nveg", "agua", "soja", "arroz", "temp")) %>% 
    dplyr::mutate(categoryName = as.factor(categoryName))
tb_legend

# diagrama de sankey
diagrama_sankey <- sankeyLand(dataset = ct$lulc_Multistep,
                              legendtable = tb_legend)
diagrama_sankey

# export .html
networkD3::saveNetwork(diagrama_sankey, "04_results/diagrama_sankey.html")

# export .png
webshot2::webshot(url = "04_results/diagrama_sankey.html", 
                  file = "04_results/diagrama_sankey.png", 
                  vwidth = 1000, vheight = 900)

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

## class metrics ----
class_metrics <- landscapemetrics::list_lsm() %>%
    dplyr::filter(level == "class") %>% 
    dplyr::arrange(type)
class_metrics

class_metrics_type <- class_metrics %>%
    dplyr::group_by(type) %>% 
    dplyr::summarise(n = n())
class_metrics_type

## landscape metrics ----
landscape_metrics <- landscapemetrics::list_lsm() %>%
    dplyr::filter(level == "landscape") %>% 
    dplyr::arrange(type)
landscape_metrics

landscape_metrics_type <- landscape_metrics %>%
    dplyr::group_by(type) %>% 
    dplyr::summarise(n = n())
landscape_metrics_type

# calcular as metricas ----------------------------------------------------

#' estrutura das funcoes
#' 1. prefixo: ‘lsm_’
#' 2. nivel: ‘p’, ‘c’ e ‘l’ para patch, class e landscape level
#' 3. metrica: patch area - ‘lsm_p_area’
#' 4. todas as funcoes funcionam para rasterlayers, rasterstack/rasterbrick ou list
#' 5. algumas funcoes permitem add parametros: edge depth ou cell neighbourhood rule


## area no nivel de mancha (patch - p) ----
mapbiomas_1985_santa_maria_utm_area_p <- landscapemetrics::lsm_p_area(landscape = mapbiomas_1985_santa_maria_utm)
mapbiomas_1985_santa_maria_utm_area_p

mapbiomas_2023_santa_maria_utm_area_p <- landscapemetrics::lsm_p_area(landscape = mapbiomas_2023_santa_maria_utm)
mapbiomas_2023_santa_maria_utm_area_p


## area no nivel de classe (class - c) ----
mapbiomas_1985_santa_maria_utm_area_c <- landscapemetrics::lsm_c_area_mn(landscape = mapbiomas_1985_santa_maria_utm)
mapbiomas_1985_santa_maria_utm_area_c

mapbiomas_2023_santa_maria_utm_area_c <- landscapemetrics::lsm_c_area_mn(landscape = mapbiomas_2023_santa_maria_utm)
mapbiomas_2023_santa_maria_utm_area_c


## area no nivel de paisagem (landscape - l) ----
mapbiomas_1985_santa_maria_utm_area_l <- landscapemetrics::lsm_l_area_mn(landscape = mapbiomas_1985_santa_maria_utm)
mapbiomas_1985_santa_maria_utm_area_l

# calcular todas as metricas por nivel ------------------------------------

#' calculate_lsm()
#' calcula varias metricas simultaneamente
#' facilita a entrada de parametros
#' permite escolha por ‘level’, ‘metric’, ‘name’, ‘type’, ‘what’

## calcular multiplas metricas ----
lsm_multiplas_metricas_1985 <- landscapemetrics::calculate_lsm(
    landscape = mapbiomas_1985_santa_maria_utm, 
    metric = c("area", "core", "enn"), 
    level = "class",
    edge_depth = 1, # borda
    neighbourhood = 8, # oito celulas nas vizinhancas
    full_name = TRUE, 
    verbose = TRUE, 
    progress = TRUE)
lsm_multiplas_metricas_1985

lsm_multiplas_metricas_2023 <- landscapemetrics::calculate_lsm(
    landscape = mapbiomas_2023_santa_maria_utm, 
    metric = c("area", "core", "enn"), 
    level = "class",
    edge_depth = 1, # borda
    neighbourhood = 8, # oito celulas nas vizinhancas
    full_name = TRUE, 
    verbose = TRUE, 
    progress = TRUE)
lsm_multiplas_metricas_2023

## variacao temporal ----
lsm_multiplas_metricas_1985_filter <- lsm_multiplas_metricas_1985 %>% 
    dplyr::filter(class %in% c(3, 12),
                  metric %in% c("area_mn", "core_mn", "enn_mn")) %>% 
    dplyr::mutate(year = "metric_1985") %>% 
    dplyr::select(year, class, metric, value)
lsm_multiplas_metricas_1985_filter

lsm_multiplas_metricas_2023_filter <- lsm_multiplas_metricas_2023 %>% 
    dplyr::filter(class %in% c(3, 12),
                  metric %in% c("area_mn", "core_mn", "enn_mn")) %>% 
    dplyr::mutate(year = "metric_2023") %>% 
    dplyr::select(year, class, metric, value)
lsm_multiplas_metricas_2023_filter

lsm_multiplas_metricas_temporal <- lsm_multiplas_metricas_1985_filter %>% 
    dplyr::bind_rows(lsm_multiplas_metricas_2023_filter) %>% 
    dplyr::mutate(class = case_when(class == 3 ~ "forest",
                                    class == 12 ~ "grassland")) %>% 
    tidyr::pivot_wider(names_from = year, values_from = value) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(variation = (metric_2023 - metric_1985)/metric_1985 * 100)
lsm_multiplas_metricas_temporal

writexl::write_xlsx(lsm_multiplas_metricas_temporal, "04_results/lsm_multiplas_metricas_temporal.xlsx")

# espacializar as metricas ------------------------------------

## reclassificar ----
mapbiomas_1985_santa_maria_utm_forest <- terra::ifel(mapbiomas_1985_santa_maria_utm == 3, 1, NA)
mapbiomas_1985_santa_maria_utm_forest

plot(mapbiomas_1985_santa_maria_utm_forest, col = "forestgreen", legend = FALSE)
plot(santa_maria_utm, add = TRUE)

mapbiomas_1985_santa_maria_utm_grassland <- terra::ifel(mapbiomas_1985_santa_maria_utm == 12, 1, NA)
mapbiomas_1985_santa_maria_utm_grassland

plot(mapbiomas_1985_santa_maria_utm_grassland, col = "orange", legend = FALSE)
plot(santa_maria_utm, add = TRUE)

mapbiomas_2023_santa_maria_utm_forest <- terra::ifel(mapbiomas_2023_santa_maria_utm == 3, 1, NA)
mapbiomas_2023_santa_maria_utm_forest

plot(mapbiomas_2023_santa_maria_utm_forest, col = "forestgreen", legend = FALSE)
plot(santa_maria_utm, add = TRUE)

mapbiomas_2023_santa_maria_utm_grassland <- terra::ifel(mapbiomas_2023_santa_maria_utm == 12, 1, NA)
mapbiomas_2023_santa_maria_utm_grassland

plot(mapbiomas_2023_santa_maria_utm_grassland, col = "orange", legend = FALSE)
plot(santa_maria_utm, add = TRUE)


## calcular e espacializar as metricas ----
mapbiomas_1985_santa_maria_utm_forest_area <- landscapemetrics::spatialize_lsm(
    landscape = mapbiomas_1985_santa_maria_utm_forest,
    metric = "area",
    progress = TRUE)
mapbiomas_1985_santa_maria_utm_forest_area

mapbiomas_1985_santa_maria_utm_grassland_area <- landscapemetrics::spatialize_lsm(
    landscape = mapbiomas_1985_santa_maria_utm_grassland,
    metric = "area",
    progress = TRUE)
mapbiomas_1985_santa_maria_utm_grassland_area


mapbiomas_2023_santa_maria_utm_forest_area <- landscapemetrics::spatialize_lsm(
    landscape = mapbiomas_2023_santa_maria_utm_forest,
    metric = "area",
    progress = TRUE)
mapbiomas_2023_santa_maria_utm_forest_area

mapbiomas_2023_santa_maria_utm_grassland_area <- landscapemetrics::spatialize_lsm(
    landscape = mapbiomas_2023_santa_maria_utm_grassland,
    metric = "area",
    progress = TRUE)
mapbiomas_2023_santa_maria_utm_grassland_area

## mapas ----
map_1985_forest_area <- tm_shape(mapbiomas_1985_santa_maria_utm_forest_area$layer_1$lsm_p_area) +
    tm_raster(col = "value", 
              col.scale = tm_scale_continuous_log10(values = "matplotlib.greens"),
              col.legend = tm_legend(title = "Área (ha)",
                                     position = tm_pos_out("right", "center"), 
                                     reverse = TRUE)) +
    tm_shape(santa_maria_utm) +
    tm_borders() +
    tm_compass(size = 3, position = c(.9, .2)) +
    tm_scalebar(text.size = 1, breaks = c(0, 5, 10), position = c(.75, .1))
map_1985_forest_area
tmap_save(map_1985_forest_area, "04_results/map_1985_forest_area.png", width = 30, height = 20, units = "cm", dpi = 300)

map_1985_grassland_area <- tm_shape(mapbiomas_1985_santa_maria_utm_grassland_area$layer_1$lsm_p_area) +
    tm_raster(col = "value", 
              col.scale = tm_scale_continuous_log10(values = "matplotlib.oranges"),
              col.legend = tm_legend(title = "Área (ha)",
                                     position = tm_pos_out("right", "center"), 
                                     reverse = TRUE)) +
    tm_shape(santa_maria_utm) +
    tm_borders() +
    tm_compass(size = 3, position = c(.9, .2)) +
    tm_scalebar(text.size = 1, breaks = c(0, 5, 10), position = c(.75, .1))
map_1985_grassland_area
tmap_save(map_1985_grassland_area, "04_results/map_1985_grassland_area.png", width = 30, height = 20, units = "cm", dpi = 300)

map_2023_forest_area <- tm_shape(mapbiomas_2023_santa_maria_utm_forest_area$layer_1$lsm_p_area) +
    tm_raster(col = "value", 
              col.scale = tm_scale_continuous_log10(values = "matplotlib.greens"),
              col.legend = tm_legend(title = "Área (ha)",
                                     position = tm_pos_out("right", "center"), 
                                     reverse = TRUE)) +
    tm_shape(santa_maria_utm) +
    tm_borders() +
    tm_compass(size = 3, position = c(.9, .2)) +
    tm_scalebar(text.size = 1, breaks = c(0, 5, 10), position = c(.75, .1))
map_2023_forest_area
tmap_save(map_2023_forest_area, "04_results/map_2023_forest_area.png", width = 30, height = 20, units = "cm", dpi = 300)

map_2023_grassland_area <- tm_shape(mapbiomas_2023_santa_maria_utm_grassland_area$layer_1$lsm_p_area) +
    tm_raster(col = "value", 
              col.scale = tm_scale_continuous_log10(values = "matplotlib.oranges"),
              col.legend = tm_legend(title = "Área (ha)",
                                     position = tm_pos_out("right", "center"), 
                                     reverse = TRUE)) +
    tm_shape(santa_maria_utm) +
    tm_borders() +
    tm_compass(size = 3, position = c(.9, .2)) +
    tm_scalebar(text.size = 1, breaks = c(0, 5, 10), position = c(.75, .1))
map_2023_grassland_area
tmap_save(map_2023_grassland_area, "04_results/map_2023_grassland_area.png", width = 30, height = 20, units = "cm", dpi = 300)

# end ---------------------------------------------------------------------