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



test <- slope_data_analysis %>%
  cbind(EcoregionMask_hex[st_nearest_feature(slope_data_analysis, EcoregionMask_hex),]) %>%
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
  dplyr::do(model = randomForest::randomForest(formula = area_slope ~
                                                 cloud_slope +
                                                 humidity_slope +
                                                 population_slope +
                                                 temp_slope +
                                                 snow_slope +
                                                 precip_slope +
                                                 wshd_area +
                                                 slope_100 +
                                                 elevation +
                                                 shore_dev,
                                               data = ., na.action=na.roughfix,
                                               importance = T)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

e <- as.data.frame(rf_boreal$model[[1]]$importance[,1])
e$predictor <- row.names(e)
e$NSE = hydroGOF::NSE(rf_boreal$model[[1]]$predicted, rf_boreal$model[[1]]$y)

e <- e %>% arrange(-`rf_boreal$model[[1]]$importance[, 1]`) %>%
  rename(incMSE = `rf_boreal$model[[1]]$importance[, 1]`)

rf_boreal_df <- e %>%
  mutate(predictor_new = case_when(
    predictor == "fit_humid_slope" ~ "Δ Humidity",
    predictor == "fit_lw_slope" ~ "Δ Longwave",
    predictor == "fit_pop_slope" ~ "Δ Population",
    predictor == "fit_precip_slope" ~ "Δ Precipitation",
    predictor == "fit_snow_slope" ~ "Δ Snowfall",
    predictor == "fit_temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    TRUE ~ NA_character_)) %>%
  mutate(incMSE = round(incMSE, digits = 3))

boreal_rf_figure <- ggplot(rf_boreal_df, aes(y = incMSE, x = NA, group = predictor_new))+
  geom_bar(aes(fill = predictor_new),stat = "identity")+
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T)+
  geom_text(aes(label=incMSE),color="black",size=3,
            position=position_stack(vjust=0.5))+
  labs(title = "Boreal/Tundra RF")+
  theme_minimal()






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
  dplyr::do(model = randomForest::randomForest(formula = area_slope ~
                                                 cloud_slope +
                                                 humidity_slope +
                                                 population_slope +
                                                 temp_slope +
                                                 snow_slope +
                                                 precip_slope +
                                                 wshd_area +
                                                 slope_100 +
                                                 elevation +
                                                 shore_dev,
                                               data = ., na.action=na.roughfix)) %>%
  dplyr::collect() %>%
  dplyr::ungroup(.)

e <- as.data.frame(rf_desert$model[[1]]$importance[,1])
e$predictor <- row.names(e)
e$NSE = hydroGOF::NSE(rf_desert$model[[1]]$predicted, rf_desert$model[[1]]$y)

e <- e %>% arrange(-`rf_desert$model[[1]]$importance[, 1]`) %>%
  rename(incMSE = `rf_desert$model[[1]]$importance[, 1]`)

rf_desert_df <- e %>%
  mutate(predictor_new = case_when(
    predictor == "fit_humid_slope" ~ "Δ Humidity",
    predictor == "fit_lw_slope" ~ "Δ Longwave",
    predictor == "fit_pop_slope" ~ "Δ Population",
    predictor == "fit_precip_slope" ~ "Δ Precipitation",
    predictor == "fit_snow_slope" ~ "Δ Snowfall",
    predictor == "fit_temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    TRUE ~ NA_character_)) %>%
  mutate(incMSE = round(incMSE, digits = 3))

desert_rf_figure <- ggplot(rf_desert_df, aes(y = incMSE, x = NA, group = predictor_new))+
  geom_bar(aes(fill = predictor_new),stat = "identity")+
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T)+
  geom_text(aes(label=incMSE),color="black",size=3,
            position=position_stack(vjust=0.5))+
  labs(title = "desert RF")+
  theme_minimal()







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
  dplyr::do(model = randomForest::randomForest(formula = area_slope ~
                                                 cloud_slope +
                                                 humidity_slope +
                                                 population_slope +
                                                 temp_slope +
                                                 snow_slope +
                                                 precip_slope +
                                                 wshd_area +
                                                 slope_100 +
                                                 elevation +
                                                 shore_dev,
                                               data = ., na.action=na.roughfix)) %>%
  dplyr::collect() %>%
  dplyr::ungroup(.)

e <- as.data.frame(rf_temperate$model[[1]]$importance[,1])
e$predictor <- row.names(e)
e$NSE = hydroGOF::NSE(rf_temperate$model[[1]]$predicted, rf_temperate$model[[1]]$y)

e <- e %>% arrange(-`rf_temperate$model[[1]]$importance[, 1]`) %>%
  rename(incMSE = `rf_temperate$model[[1]]$importance[, 1]`)

rf_temperate_df <- e %>%
  mutate(predictor_new = case_when(
    predictor == "fit_humid_slope" ~ "Δ Humidity",
    predictor == "fit_lw_slope" ~ "Δ Longwave",
    predictor == "fit_pop_slope" ~ "Δ Population",
    predictor == "fit_precip_slope" ~ "Δ Precipitation",
    predictor == "fit_snow_slope" ~ "Δ Snowfall",
    predictor == "fit_temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    TRUE ~ NA_character_)) %>%
  mutate(incMSE = round(incMSE, digits = 3))

temperate_rf_figure <- ggplot(rf_temperate_df, aes(y = incMSE, x = NA, group = predictor_new))+
  geom_bar(aes(fill = predictor_new),stat = "identity")+
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T)+
  geom_text(aes(label=incMSE),color="black",size=3,
            position=position_stack(vjust=0.5))+
  labs(title = "temperate RF")+
  theme_minimal()




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
  dplyr::do(model = randomForest::randomForest(formula = area_slope ~
                                                 cloud_slope +
                                                 humidity_slope +
                                                 population_slope +
                                                 temp_slope +
                                                 snow_slope +
                                                 precip_slope +
                                                 wshd_area +
                                                 slope_100 +
                                                 elevation +
                                                 shore_dev,
                                               data = ., na.action=na.roughfix)) %>%
  dplyr::collect() %>%
  dplyr::ungroup(.)

e <- as.data.frame(rf_tropical$model[[1]]$importance[,1])
e$predictor <- row.names(e)
e$NSE = hydroGOF::NSE(rf_tropical$model[[1]]$predicted, rf_tropical$model[[1]]$y)

e <- e %>% arrange(-`rf_tropical$model[[1]]$importance[, 1]`) %>%
  rename(incMSE = `rf_tropical$model[[1]]$importance[, 1]`)

rf_tropical_df <- e %>%
  mutate(predictor_new = case_when(
    predictor == "fit_humid_slope" ~ "Δ Humidity",
    predictor == "fit_lw_slope" ~ "Δ Longwave",
    predictor == "fit_pop_slope" ~ "Δ Population",
    predictor == "fit_precip_slope" ~ "Δ Precipitation",
    predictor == "fit_snow_slope" ~ "Δ Snowfall",
    predictor == "fit_temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    TRUE ~ NA_character_)) %>%
  mutate(incMSE = round(incMSE, digits = 3))

tropical_rf_figure <- ggplot(rf_tropical_df, aes(y = incMSE, x = NA, group = predictor_new))+
  geom_bar(aes(fill = predictor_new),stat = "identity")+
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T)+
  geom_text(aes(label=incMSE),color="black",size=3,
            position=position_stack(vjust=0.5))+
  labs(title = "tropical RF")+
  theme_minimal()
