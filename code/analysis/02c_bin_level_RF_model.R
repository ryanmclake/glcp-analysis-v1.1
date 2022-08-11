slope_data_analysis_high <- slope_data %>%
  mutate(sig_lake_change = ifelse(lsa_pval<=0.05, "YES","NO"))%>%
  filter(sig_lake_change == "YES")
%>%
  mutate(q75 = quantile(lsa_tau, 0.75000000)) %>%
  filter(lsa_tau >= q75)%>%
  select(-q75)

slope_data_analysis_low <- slope_data %>%
  mutate(sig_lake_change = ifelse(lsa_pval<=0.05, "YES","NO"))%>%
  filter(sig_lake_change == "YES") %>%
  mutate(q25 = quantile(lsa_tau, 0.25000000)) %>%
  filter(lsa_tau <= q25) %>%
  select(-q25)

slope_data_analysis_LR <- rbind(slope_data_analysis_high,slope_data_analysis_low) %>%
  mutate(direction = ifelse(lsa_tau < 0, 0, 1))

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


######################################
### HEX LEVEL RANDOM FOREST MODELS ###
######################################

world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid_spacing <- 1010000 # CRS units in meters (100000 m = 111 km & 111 km ~ 1 Decimal degree)

grid <- st_make_grid(
  world,
  cellsize = c(grid_spacing, grid_spacing),
  #n = c(200, 200), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  square = FALSE) %>%
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid)

area_hexes <- st_join(slope_data, grid, join = st_intersects)

test <- area_hexes %>%
  cbind(EcoregionMask_hex[st_nearest_feature(area_hexes, EcoregionMask_hex),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))

test$dist <- drop_units(test$dist)
test <- test %>% filter(dist == 0) %>% arrange(hylak_id)

area_hexes_avg <- test %>%
  st_drop_geometry() %>%
  group_by(index, hybas_id) %>%
  summarise(depth_avg = median(depth_avg, na.rm = TRUE),
            elevation = median(elevation, na.rm = TRUE),
            lsa_pval = median(lsa_pval, na.rm = TRUE),
            lsa_tau = median(lsa_tau, na.rm = TRUE),
            precip_pval = median(precip_pval, na.rm = TRUE),
            precip_tau = median(precip_tau, na.rm = TRUE),
            temp_pval = median(temp_pval, na.rm = TRUE),
            temp_tau = median(temp_tau, na.rm = TRUE),
            pop_pval = median(pop_pval, na.rm = TRUE),
            pop_tau = median(pop_tau, na.rm = TRUE),
            snow_pval = median(snow_pval, na.rm = TRUE),
            snow_tau = median(snow_tau, na.rm = TRUE)) %>%
  right_join(grid, by="index") %>%
  st_sf() %>%
  na.omit(.)

hex_rf <- list()
indexes <- unique(area_hexes_avg$index)

for(i in 1:length(indexes)){
  b <- area_hexes_avg %>% filter(index == indexes[i]) %>%
    dplyr::do(model = randomForest::randomForest(formula = lsa_tau ~
                                                   precip_tau +
                                                   temp_tau +
                                                   pop_tau +
                                                   snow_tau,
                                                   data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)
  e$index = indexes[i]

    e <- e %>% arrange(-IncNodePurity)

  hex_rf[[i]] <- e

}

hex_level_rf = do.call(rbind, hex_rf) %>%
  group_by(index) %>%
  slice(1) %>%
  mutate(strong_NSE = ifelse(NSE>=0, "STRONG","WEAK"))%>%
  right_join(grid, by = "index")%>%
  st_sf()

lake_change_predictors <- hex_level_rf %>%
  mutate(predictor_new = case_when(
    predictor == "pop_tau" ~ "Δ Population",
    predictor == "precip_tau" ~ "Δ Precipitation",
    predictor == "snow_tau" ~ "Δ Snowfall",
    predictor == "temp_tau" ~ "Δ Temperature",
    TRUE ~ NA_character_))

lake_change_predictors_plot <- ggplot(lake_change_predictors) +
  geom_sf(data = world, color = "black", lwd = 0.5)+
  geom_sf(lwd = 0.05,
          aes(fill = predictor_new),color = "white")+
  # geom_sf(data = shp_boreal,lwd = 0, color = "black", fill = "black", alpha = 0.25)+
  # geom_sf(data = shp_desert,lwd = 0, color = "firebrick4", fill = "firebrick4", alpha = 0.25)+
  # geom_sf(data = shp_temperate,lwd = 0, color = "forestgreen", fill = "forestgreen", alpha = 0.25)+
  # geom_sf(data = shp_tropical,lwd = 0, color = "magenta3", fill = "magenta3", alpha = 0.25)+
  scale_fill_viridis(option = "C", na.value = "white",
    direction = -1, discrete = T, name = "**Basin-level Predictor** <br> Random Forest Model")+
  #scale_color_manual(values = c("blue", NA, "black"), na.value = "black",name = "**Predictive Skill** <br> NSE") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  #guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.09, 0.2),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


lake_change_predictors_bar <- ggplot(lake_change_predictors, aes(x = predictor_new)) +
  geom_bar(aes(fill = predictor_new), color = "black") +
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T, name = "Predictor Count") +
  labs(title = "Global Predictors")+
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 5, color = "black"),
        axis.title.y = element_text(size = 5, color = "black"),
        plot.title = element_text(size=5),
        line = element_line(color = "black"))+
  ylab("Hex Count")

p1 <- viewport(width = 0.12, height = 0.17, x = 0.36, y = 0.65)
p2 <- viewport(width = 0.12, height = 0.17, x = 0.72, y = 0.42)
p3 <- viewport(width = 0.12, height = 0.17, x = 0.45, y = 0.4)
p4 <- viewport(width = 0.12, height = 0.17, x = 0.11, y = 0.53)

#Just draw the plot twice

png(filename = "./output/figures/HEX_level_predictors_STRONGNSE.png",
    width     = 10.41,
    height    = 7.29,
    units     = "in",
    res       = 1000,
    pointsize = 12,
    bg = "white")
print(lake_change_predictors_plot)
print(lake_change_predictors_bar, vp = p1)
dev.off()












hex_boreal <- test %>%
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
  mutate(q95 = quantile(area_slope, 0.999), row = row_number()) %>%
  filter(area_slope <= q95) %>%
  mutate(q5 = quantile(area_slope, 0.001), row = row_number()) %>%
  filter(area_slope >= q5) %>%
  group_by(index, hybas_id) %>%
  summarise(shore_dev = median(shore_dev, na.rm = TRUE),
            depth_avg = median(depth_avg, na.rm = TRUE),
            res_time = median(res_time, na.rm = TRUE),
            p_val = median(sig_test, na.rm = TRUE),
            elevation = median(elevation, na.rm = TRUE),
            slope_100 = median(slope_100, na.rm = TRUE),
            area_slope = median(area_slope, na.rm = TRUE),
            precip_slope = median(precip_slope, na.rm = TRUE),
            snow_slope = median(snow_slope, na.rm = TRUE),
            temp_slope = median(temp_slope, na.rm = TRUE),
            population_slope = median(population_slope, na.rm = TRUE),
            humidity_slope = median(humidity_slope, na.rm = TRUE),
            cloud_slope = median(cloud_slope, na.rm = TRUE)) %>%
  mutate(sig_lake_change = ifelse(p_val<=0.05, "YES","NO"))%>%
  right_join(grid, by="index") %>%
  st_sf() %>%
  na.omit(.)

hex_rf_boreal <- list()
indexes <- unique(hex_boreal$index)

for(i in 1:length(indexes)){
  b <- hex_boreal %>% filter(index == indexes[i]) %>%
    dplyr::do(model = randomForest::randomForest(formula = area_slope ~
                                                   humidity_slope +
                                                   population_slope +
                                                   temp_slope +
                                                   snow_slope +
                                                   precip_slope,
                                                 data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)
  e$index = indexes[i]

  e <- e %>% arrange(-IncNodePurity)

  hex_rf_boreal[[i]] <- e

}

hex_level_rf_boreal = do.call(rbind, hex_rf_boreal) %>%
  group_by(index) %>%
  slice(1) %>%
  mutate(strong_NSE = ifelse(NSE>=0, "STRONG","WEAK"))%>%
  right_join(grid, by = "index")%>%
  filter(strong_NSE == "STRONG")%>%
  st_sf()

boreal_change_predictors <-hex_level_rf_boreal %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))%>%
  ggplot(.) +
  geom_sf(data = world, color = "black", lwd = 2)+
  geom_sf(lwd = 0.4,
          aes(fill = predictor_new),color = "white")+
  geom_sf(data = shp_boreal,lwd = 0, color = "black", fill = "black", alpha = 0.25)+
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T, name = "**Hybro Basin Predictor** <br> Random Forest")+
  #scale_color_manual(values = c("blue", NA, "black"), na.value = "black",name = "**Predictive Skill** <br> NSE") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  #guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

boreal_bar <- hex_level_rf_boreal %>% select(predictor) %>% na.omit(.) %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))%>%
  ggplot(., aes(x = predictor_new)) +
  geom_bar(aes(fill = predictor_new), color = "black") +
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T, name = "Predictor Count") +
  labs(title = "Boreal Predictors")+
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 5, color = "black"),
        axis.title.y = element_text(size = 5, color = "black"),
        plot.title = element_text(size=5),
        line = element_line(color = "black"))+
  ylab("Hex Count")




hex_desert <- test %>%
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
  mutate(q95 = quantile(area_slope, 0.999), row = row_number()) %>%
  filter(area_slope <= q95) %>%
  mutate(q5 = quantile(area_slope, 0.001), row = row_number()) %>%
  filter(area_slope >= q5) %>%
  group_by(index, hybas_id) %>%
  summarise(shore_dev = median(shore_dev, na.rm = TRUE),
            depth_avg = median(depth_avg, na.rm = TRUE),
            res_time = median(res_time, na.rm = TRUE),
            p_val = median(sig_test, na.rm = TRUE),
            elevation = median(elevation, na.rm = TRUE),
            slope_100 = median(slope_100, na.rm = TRUE),
            area_slope = median(area_slope, na.rm = TRUE),
            precip_slope = median(precip_slope, na.rm = TRUE),
            snow_slope = median(snow_slope, na.rm = TRUE),
            temp_slope = median(temp_slope, na.rm = TRUE),
            population_slope = median(population_slope, na.rm = TRUE),
            humidity_slope = median(humidity_slope, na.rm = TRUE),
            cloud_slope = median(cloud_slope, na.rm = TRUE)) %>%
  mutate(sig_lake_change = ifelse(p_val<=0.05, "YES","NO"))%>%
  right_join(grid, by="index") %>%
  st_sf() %>%
  na.omit(.)

hex_rf_desert <- list()
indexes <- unique(hex_desert$index)

for(i in 1:length(indexes)){
  b <- hex_desert %>% filter(index == indexes[i]) %>%
    dplyr::do(model = randomForest::randomForest(formula = area_slope ~
                                                   humidity_slope +
                                                   population_slope +
                                                   temp_slope +
                                                   snow_slope +
                                                   precip_slope,
                                                 data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)
  e$index = indexes[i]

  e <- e %>% arrange(-IncNodePurity)

  hex_rf_desert[[i]] <- e

}

hex_level_rf_desert = do.call(rbind, hex_rf_desert) %>%
  group_by(index) %>%
  slice(1) %>%
  mutate(strong_NSE = ifelse(NSE>=0, "STRONG","WEAK"))%>%
  right_join(grid, by = "index")%>%
  filter(strong_NSE == "STRONG")%>%
  st_sf()

desert_change_predictors <-hex_level_rf_desert %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))%>%
  ggplot(.) +
  geom_sf(lwd = 0.4,
          aes(fill = predictor_new, color = strong_NSE))+
  scale_fill_viridis(option = "C", na.value = "black",
                     direction = -1, discrete = T, name = "**Hybro Basin Predictor** <br> Random Forest")+
  scale_color_manual(values = c("blue", NA, "black"), na.value = "black",name = "**Predictive Skill** <br> NSE") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  #guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

desert_bar <- hex_level_rf_desert %>% na.omit(.) %>% select(predictor) %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))%>%
  ggplot(., aes(x = predictor_new)) +
  geom_bar(aes(fill = predictor_new), color = "black") +
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T, name = "Predictor Count") +
  labs(title = "Desert Predictors")+
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 5, color = "black"),
        axis.title.y = element_text(size = 5, color = "black"),
        plot.title = element_text(size=5),
        line = element_line(color = "black"))+
  ylab("Hex Count")


hex_temperate <- test %>%
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
  mutate(q95 = quantile(area_slope, 0.999), row = row_number()) %>%
  filter(area_slope <= q95) %>%
  mutate(q5 = quantile(area_slope, 0.001), row = row_number()) %>%
  filter(area_slope >= q5) %>%
  group_by(index, hybas_id) %>%
  summarise(shore_dev = median(shore_dev, na.rm = TRUE),
            depth_avg = median(depth_avg, na.rm = TRUE),
            res_time = median(res_time, na.rm = TRUE),
            p_val = median(sig_test, na.rm = TRUE),
            elevation = median(elevation, na.rm = TRUE),
            slope_100 = median(slope_100, na.rm = TRUE),
            area_slope = median(area_slope, na.rm = TRUE),
            precip_slope = median(precip_slope, na.rm = TRUE),
            snow_slope = median(snow_slope, na.rm = TRUE),
            temp_slope = median(temp_slope, na.rm = TRUE),
            population_slope = median(population_slope, na.rm = TRUE),
            humidity_slope = median(humidity_slope, na.rm = TRUE),
            cloud_slope = median(cloud_slope, na.rm = TRUE)) %>%
  mutate(sig_lake_change = ifelse(p_val<=0.05, "YES","NO"))%>%
  right_join(grid, by="index") %>%
  st_sf() %>%
  na.omit(.)

hex_rf_temperate <- list()
indexes <- unique(hex_temperate$index)

for(i in 1:length(indexes)){
  b <- hex_temperate %>% filter(index == indexes[i]) %>%
    dplyr::do(model = randomForest::randomForest(formula = area_slope ~
                                                   humidity_slope +
                                                   population_slope +
                                                   temp_slope +
                                                   snow_slope +
                                                   precip_slope,
                                                 data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)
  e$index = indexes[i]

  e <- e %>% arrange(-IncNodePurity)

  hex_rf_temperate[[i]] <- e

}

hex_level_rf_temperate = do.call(rbind, hex_rf_temperate) %>%
  group_by(index) %>%
  slice(1) %>%
  mutate(strong_NSE = ifelse(NSE>=0, "STRONG","WEAK"))%>%
  right_join(grid, by = "index")%>%
  filter(strong_NSE == "STRONG")%>%
  st_sf()

temperate_change_predictors <-hex_level_rf_temperate %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))%>%
  ggplot(.) +
  geom_sf(lwd = 0.4,
          aes(fill = predictor_new, color = strong_NSE))+
  scale_fill_viridis(option = "C", na.value = "black",
                     direction = -1, discrete = T, name = "**Hybro Basin Predictor** <br> Random Forest")+
  scale_color_manual(values = c("blue", NA, "black"), na.value = "black",name = "**Predictive Skill** <br> NSE") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  #guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

temperate_bar <- hex_level_rf_temperate %>% select(predictor) %>% na.omit(.) %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))%>%
  ggplot(., aes(x = predictor_new)) +
  geom_bar(aes(fill = predictor_new), color = "black") +
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T, name = "Predictor Count") +
  labs(title = "Temperate Predictors")+
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 5, color = "black"),
        axis.title.y = element_text(size = 5, color = "black"),
        plot.title = element_text(size=5),
        line = element_line(color = "black"))+
  ylab("Hex Count")



hex_tropical <- test %>%
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
  filter(biome_type %in% c("TROPICAL MOIST FOREST","TROPICAL DRY FOREST","TROPICAL GRASSLAND","MANGROVES",
                           "TROPICAL CONIFEROUS FOREST"))%>%
  st_drop_geometry() %>%
  mutate(q95 = quantile(area_slope, 0.999), row = row_number()) %>%
  filter(area_slope <= q95) %>%
  mutate(q5 = quantile(area_slope, 0.001), row = row_number()) %>%
  filter(area_slope >= q5) %>%
  group_by(index, hybas_id) %>%
  summarise(shore_dev = median(shore_dev, na.rm = TRUE),
            depth_avg = median(depth_avg, na.rm = TRUE),
            res_time = median(res_time, na.rm = TRUE),
            p_val = median(sig_test, na.rm = TRUE),
            elevation = median(elevation, na.rm = TRUE),
            slope_100 = median(slope_100, na.rm = TRUE),
            area_slope = median(area_slope, na.rm = TRUE),
            precip_slope = median(precip_slope, na.rm = TRUE),
            snow_slope = median(snow_slope, na.rm = TRUE),
            temp_slope = median(temp_slope, na.rm = TRUE),
            population_slope = median(population_slope, na.rm = TRUE),
            humidity_slope = median(humidity_slope, na.rm = TRUE),
            cloud_slope = median(cloud_slope, na.rm = TRUE)) %>%
  mutate(sig_lake_change = ifelse(p_val<=0.05, "YES","NO"))%>%
  right_join(grid, by="index") %>%
  st_sf() %>%
  na.omit(.)

hex_rf_tropical <- list()
indexes <- unique(hex_tropical$index)

for(i in 1:length(indexes)){
  b <- hex_tropical %>% filter(index == indexes[i]) %>%
    dplyr::do(model = randomForest::randomForest(formula = area_slope ~
                                                   humidity_slope +
                                                   population_slope +
                                                   temp_slope +
                                                   snow_slope +
                                                   precip_slope,
                                                   data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)
  e$index = indexes[i]

  e <- e %>% arrange(-IncNodePurity)

  hex_rf_tropical[[i]] <- e

}

hex_level_rf_tropical = do.call(rbind, hex_rf_tropical) %>%
  group_by(index) %>%
  slice(1) %>%
  mutate(strong_NSE = ifelse(NSE>=0, "STRONG","WEAK"))%>%
  right_join(grid, by = "index")%>%
  filter(strong_NSE == "STRONG")%>%
  st_sf()

tropical_change_predictors <- hex_level_rf_tropical %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))%>%
  ggplot(.) +
  geom_sf(lwd = 0.4,
          aes(fill = predictor_new, color = strong_NSE))+
  scale_fill_viridis(option = "C", na.value = "black",
                     direction = -1, discrete = T, name = "**Hybro Basin Predictor** <br> Random Forest")+
  scale_color_manual(values = c("blue", NA, "black"), na.value = "black",name = "**Predictive Skill** <br> NSE") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  #guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

tropical_bar <- hex_level_rf_tropical %>% select(predictor) %>% na.omit(.) %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))%>%
  ggplot(., aes(x = predictor_new)) +
  geom_bar(aes(fill = predictor_new), color = "black") +
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T, name = "Predictor Count") +
  labs(title = "Tropical Predictors")+
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 5, color = "black"),
        axis.title.y = element_text(size = 5, color = "black"),
        plot.title = element_text(size=5),
        line = element_line(color = "black"))+
  ylab("Hex Count")





### Big figure ###

ecoregion_change_predictors <-
  rbind(hex_level_rf_tropical, hex_level_rf_boreal, hex_level_rf_desert, hex_level_rf_temperate) %>%
  mutate(predictor_new = case_when(
    predictor == "humidity_slope" ~ "Δ Humidity",
    predictor == "cloud_slope" ~ "Δ Cloud Cover",
    predictor == "population_slope" ~ "Δ Population",
    predictor == "precip_slope" ~ "Δ Precipitation",
    predictor == "snow_slope" ~ "Δ Snowfall",
    predictor == "temp_slope" ~ "Δ Temperature",
    predictor == "elevation" ~ "Elevation (m)",
    predictor == "slope_100" ~ "Near-shore slope",
    predictor == "wshd_area" ~ "Watershed Area",
    predictor == "shore_dev" ~ "Shoreline Complexity",
    TRUE ~ NA_character_))





p1 <- viewport(width = 0.12, height = 0.17, x = 0.36, y = 0.65)
p2 <- viewport(width = 0.12, height = 0.17, x = 0.72, y = 0.42)
p3 <- viewport(width = 0.12, height = 0.17, x = 0.45, y = 0.4)
p4 <- viewport(width = 0.12, height = 0.17, x = 0.11, y = 0.53)

#Just draw the plot twice

png(filename = "./output/figures/HEX_level_ecoregion_predictors_STRONGNSE.png",
    width     = 10.41,
    height    = 7.29,
    units     = "in",
    res       = 1000,
    pointsize = 12,
    bg = "white")
print(lake_change_predictors_plot)
print(temperate_bar, vp = p1)
print(desert_bar, vp = p2)
print(tropical_bar, vp = p3)
print(boreal_bar, vp = p4)
dev.off()









hex_temperate_slope <- test %>%
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
                           "MONTANE GRASSLAND","LAKE","TEMPERATE BROADLEAF FOREST")) %>%
  st_drop_geometry() %>%
  group_by(index, hybas_id) %>%
  summarise(shore_dev = mean(shore_dev, na.rm = TRUE),
            depth_avg = mean(depth_avg, na.rm = TRUE),
            res_time = mean(res_time, na.rm = TRUE),
            mk_total_p_val = median(mk_total_p_val, na.rm = TRUE),
            elevation = mean(elevation, na.rm = TRUE),
            slope_100 = mean(slope_100, na.rm = TRUE),
            Lake.Area.Change = mean(Lake.Area.Change, na.rm = TRUE),
            rsq_trends = mean(fit_total_rsq, na.rm = TRUE),
            fit_precip_slope = mean(fit_precip_slope, na.rm = TRUE),
            fit_snow_slope = mean(fit_snow_slope, na.rm = TRUE),
            fit_temp_slope = mean(fit_temp_slope, na.rm = TRUE),
            fit_pop_slope = mean(fit_pop_slope, na.rm = TRUE),
            fit_humid_slope = mean(fit_humid_slope, na.rm = TRUE),
            fit_cloud_slope = mean(fit_cloud_slope, na.rm = TRUE),
            fit_sw_slope = mean(fit_sw_slope, na.rm = TRUE),
            fit_lw_slope = mean(fit_lw_slope, na.rm = TRUE),
            wshd_area = mean(wshd_area, na.rm = TRUE)) %>%
  mutate(sig_lake_change = ifelse(mk_total_p_val<=0.05, "YES","NO"))%>%
  right_join(grid, by="index") %>%
  st_sf()

temperate_slope_density <- hex_temperate_slope %>% st_drop_geometry(.) %>%
  select(Lake.Area.Change,fit_precip_slope) %>%
  tidyr::gather(., condition, measurement, Lake.Area.Change:fit_precip_slope, factor_key=TRUE) %>%
  ggplot(., aes(x = condition, y=measurement)) +
  geom_violin(fill = "blue", alpha = 0.3)


hex_boreal_slope <- test %>%
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
  group_by(index, hybas_id) %>%
  summarise(shore_dev = mean(shore_dev, na.rm = TRUE),
            depth_avg = mean(depth_avg, na.rm = TRUE),
            res_time = mean(res_time, na.rm = TRUE),
            mk_total_p_val = median(mk_total_p_val, na.rm = TRUE),
            elevation = mean(elevation, na.rm = TRUE),
            slope_100 = mean(slope_100, na.rm = TRUE),
            Lake.Area.Change = mean(Lake.Area.Change, na.rm = TRUE),
            rsq_trends = mean(fit_total_rsq, na.rm = TRUE),
            fit_precip_slope = mean(fit_precip_slope, na.rm = TRUE),
            fit_snow_slope = mean(fit_snow_slope, na.rm = TRUE),
            fit_temp_slope = mean(fit_temp_slope, na.rm = TRUE),
            fit_pop_slope = mean(fit_pop_slope, na.rm = TRUE),
            fit_humid_slope = mean(fit_humid_slope, na.rm = TRUE),
            fit_cloud_slope = mean(fit_cloud_slope, na.rm = TRUE),
            fit_sw_slope = mean(fit_sw_slope, na.rm = TRUE),
            fit_lw_slope = mean(fit_lw_slope, na.rm = TRUE),
            wshd_area = mean(wshd_area, na.rm = TRUE)) %>%
  mutate(sig_lake_change = ifelse(mk_total_p_val<=0.05, "YES","NO"))%>%
  right_join(grid, by="index") %>%
  st_sf()

boreal_slope_density <- hex_boreal_slope %>% st_drop_geometry(.) %>%
  select(Lake.Area.Change,fit_precip_slope) %>%
  tidyr::gather(., condition, measurement, Lake.Area.Change:fit_precip_slope, factor_key=TRUE) %>%
  ggplot(., aes(x = condition, y=measurement)) +
  geom_violin(fill = "blue", alpha = 0.3)
