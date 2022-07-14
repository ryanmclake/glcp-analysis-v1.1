Dir.Base <- getwd()
Dir.Data <- file.path(Dir.Base, "data")
Dir.Shapes <- file.path(Dir.Data, "shapes")

if (!file.exists(file.path(Dir.Shapes, "WWF_ecoregions"))) {
  download.file("http://assets.worldwildlife.org/publications/15/files/original/official_teow.zip",
                destfile = file.path(Dir.Shapes, "wwf_ecoregions.zip"))
  unzip(file.path(Dir.Shapes, "wwf_ecoregions.zip"), exdir = file.path(Dir.Shapes, "WWF_ecoregions"))
}

EcoregionMask <- read_sf(file.path(Dir.Base,"data","shapes", "WWF_ecoregions", "official", "wwf_terr_ecos.shp"))
EcoregionMask <- st_make_valid(EcoregionMask)

EcoregionMask_hex <- st_make_valid(EcoregionMask)%>%
  st_transform("+proj=eqearth +wktext")

slope_data <- readRDS("./output/slopes/hylak_id_slopes.rds") %>%
  filter(sig_lake_change == "YES") %>%
  filter(lake_type == 1) %>%
  rename(`Lake Area Change` = fit_total_slope) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

test <- slope_data %>%
  cbind(EcoregionMask_hex[st_nearest_feature(slope_data, EcoregionMask_hex),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))

rf_boreal <- test %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("BOREAL FOREST","ROCK & ICE","TUNDRA")) %>%
  st_drop_geometry() %>%
  dplyr::do(model = randomForest::randomForest(formula = Lake.Area.Change ~
                                                   fit_precip_slope +
                                                   fit_snow_slope +
                                                   fit_temp_slope +
                                                   fit_pop_slope +
                                                   fit_humid_slope +
                                                   fit_lw_slope +
                                                   #shore_dev +
                                                   elevation +
                                                   slope_100 +
                                                   wshd_area,
                                                 data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

e <- as.data.frame(rf_boreal$model[[1]]$importance)
e$predictor <- row.names(e)
e$NSE = hydroGOF::NSE(rf_boreal$model[[1]]$predicted, rf_boreal$model[[1]]$y)

e <- e %>% arrange(-IncNodePurity)

rf_boreal_df <- e








rf_desert <- test %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type == "DESERT")%>%
  st_drop_geometry() %>%
  dplyr::do(model = randomForest::randomForest(formula = Lake.Area.Change ~
                                                 fit_precip_slope +
                                                 fit_snow_slope +
                                                 fit_temp_slope +
                                                 fit_pop_slope +
                                                 fit_humid_slope +
                                                 fit_lw_slope +
                                                 #shore_dev +
                                                 elevation +
                                                 slope_100 +
                                                 wshd_area,
                                               data = ., na.action=na.roughfix)) %>%
  dplyr::collect() %>%
  dplyr::ungroup(.)

e <- as.data.frame(rf_desert$model[[1]]$importance)
e$predictor <- row.names(e)
e$NSE = hydroGOF::NSE(rf_desert$model[[1]]$predicted, rf_desert$model[[1]]$y)

e <- e %>% arrange(-IncNodePurity)

rf_desert_df <- e






rf_temperate <- test %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("TEMPERATE GRASSLAND","TEMPERATE CONIFEROUS FOREST","MEDITERRANIAN FOREST","FLOODED GRASSLAND",
                           "MONTANE GRASSLAND","LAKE","TEMPERATE BROADLEAF FOREST"))%>%
  st_drop_geometry() %>%
  dplyr::do(model = randomForest::randomForest(formula = Lake.Area.Change ~
                                                 fit_precip_slope +
                                                 fit_snow_slope +
                                                 fit_temp_slope +
                                                 fit_pop_slope +
                                                 fit_humid_slope +
                                                 fit_lw_slope +
                                                 #shore_dev +
                                                 elevation +
                                                 slope_100 +
                                                 wshd_area,
                                               data = ., na.action=na.roughfix)) %>%
  dplyr::collect() %>%
  dplyr::ungroup(.)

e <- as.data.frame(rf_temperate$model[[1]]$importance)
e$predictor <- row.names(e)
e$NSE = hydroGOF::NSE(rf_temperate$model[[1]]$predicted, rf_temperate$model[[1]]$y)

e <- e %>% arrange(-IncNodePurity)

rf_temperate_df <- e






rf_tropical <- test %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_)) %>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("TROPICAL MOIST FOREST","TROPICAL DRY FOREST","TROPICAL GRASSLAND","MANGROVES",
                           "TROPICAL CONIFEROUS FOREST")) %>%
  st_drop_geometry() %>%
  dplyr::do(model = randomForest::randomForest(formula = Lake.Area.Change ~
                                                 fit_precip_slope +
                                                 fit_snow_slope +
                                                 fit_temp_slope +
                                                 fit_pop_slope +
                                                 fit_humid_slope +
                                                 fit_lw_slope +
                                                 #shore_dev +
                                                 elevation +
                                                 slope_100 +
                                                 wshd_area,
                                               data = ., na.action=na.roughfix)) %>%
  dplyr::collect() %>%
  dplyr::ungroup(.)

e <- as.data.frame(rf_tropical$model[[1]]$importance)
e$predictor <- row.names(e)
e$NSE = hydroGOF::NSE(rf_tropical$model[[1]]$predicted, rf_tropical$model[[1]]$y)

e <- e %>% arrange(-IncNodePurity)

rf_tropical_df <- e
