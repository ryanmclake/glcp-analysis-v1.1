

setwd("/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1")
Dir.Base <- getwd()
Dir.Data <- file.path(Dir.Base, "data")
Dir.Shapes <- file.path(Dir.Data, "shapes")

if (!file.exists(file.path(Dir.Shapes, "WWF_ecoregions"))) {
  download.file("http://assets.worldwildlife.org/publications/15/files/original/official_teow.zip",
                destfile = file.path(Dir.Shapes, "wwf_ecoregions.zip"))
  unzip(file.path(Dir.Shapes, "wwf_ecoregions.zip"), exdir = file.path(Dir.Shapes, "WWF_ecoregions"))
}

data <- readRDS("/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/output/slopes/hylak_id_slopes.rds") %>%
  filter(sig_lake_change == "YES") %>%
  filter(lake_type == 1) %>%
  filter(elevation >= 0) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326)

EcoregionMask <- read_sf(file.path(Dir.Base,"data","shapes", "WWF_ecoregions", "official", "wwf_terr_ecos.shp"))
EcoregionMask <- st_make_valid(EcoregionMask)

BIOME <- c(unique(EcoregionMask$BIOME))
sites <- c(unique(EcoregionMask$BIOME))
lake_to_biome <- list()

  for(i in 1:length(BIOME)){
     b <- EcoregionMask %>% filter(BIOME == sites[i])
     b2 <- data %>%
       cbind(b[st_nearest_feature(data, b),]) %>%
       mutate(dist = st_distance(geometry, geometry.1, by_element = T))
     b2$dist <- drop_units(b2$dist)
     b2 <- b2 %>% filter(dist == 0) %>% arrange(hylak_id)
     lake_to_biome[[i]] <- b2
  }

lake_to_biome = do.call(rbind, lake_to_biome)

biome_change <- lake_to_biome %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFER FOREST",
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
  filter(lake_type == 1)

biome_change_plot <- ggplot(biome_change, aes(biome_type, fit_total_slope, group = biome_type))+
  geom_boxplot(fill = "grey70", color = "black", lwd = 0.6, alpha = 0.6, show.legend = FALSE) +
  labs(x = "WWF Biomes", y = "Change in total lake area (km2)") +
  stat_summary(
    fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., color = "Mean"),
    width = .75, linetype = "dashed"
  ) +
  stat_summary(
    fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., color = "Median"),
    width = .75, linetype = "solid"
  ) +
  scale_colour_manual("", values = c(Median = "dodgerblue4", Mean = "darkred")) +
  theme_minimal() +
  theme(text = element_text(size = 14),legend.position = "top",
        panel.grid.major.y = element_line(c(0, 25, 50, 75, 100), color = "black",size = 0.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"))

biome_change_plot
ggsave(path = Dir.Base, filename = "/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/output/figures/WWF_biome_type_trends.jpg", width = 10, height = 8, device='jpg', dpi=700)

biome_rf <- list()
biomes <- unique(biome_change$biome_type)

for(i in 1:length(biomes)){
  b <- biome_change %>% filter(biome_type == biomes[i]) %>%
    dplyr::do(model = randomForest::randomForest(formula = fit_total_slope ~
                                                   fit_precip_slope +
                                                   fit_snow_slope +
                                                   fit_temp_slope +
                                                   fit_pop_slope +
                                                   fit_humid_slope +
                                                   fit_cloud_slope +
                                                   fit_sw_slope +
                                                   fit_lw_slope +
                                                   shore_dev +
                                                   elevation +
                                                   slope_100 +
                                                   wshd_area, data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)
  e$biome_type = biomes[i]

  e <- e %>% arrange(-IncNodePurity)

  biome_rf[[i]] <- e

}

biome_level_rf = do.call(rbind, biome_rf) %>%
  group_by(biome_type) %>%
  slice(1)

shp <- read_sf(paste0("/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "BROADLEAF FOREST",
    BIOME == 2 ~ "BROADLEAF FOREST",
    BIOME == 3 ~ "CONIFEROUS FOREST",
    BIOME == 4 ~ "CONIFEROUS FOREST",
    BIOME == 5 ~ "BROADLEAF FOREST",
    BIOME == 6 ~ "CONIFEROUS FOREST",
    BIOME == 7 ~ "GRASSLAND",
    BIOME == 8 ~ "GRASSLAND",
    BIOME == 9 ~ "GRASSLAND",
    BIOME == 10 ~ "GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "BROADLEAF FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext")

world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

biome_predictions <- left_join(biome_change, biome_level_rf, by = "biome_type") %>%
  st_transform("+proj=eqearth +wktext")

biome_predictions_plot <-st_join(shp,biome_predictions, join = st_intersects)%>%
  st_transform("+proj=eqearth +wktext")

biome_level_driver <-
  ggplot() +
  geom_sf(data = world, color = "black",lwd = 0.001)+
  geom_sf(data = biome_predictions_plot, color = "grey",lwd = 0.001,
          aes(fill = predictor))+
  scale_fill_viridis_d(
    option = "magma", na.value = "gray",
    direction = -1)+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  #guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

biome_level_driver
ggsave(biome_level_driver, path = ".",
       filename = "/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/output/figures/predictor_biome_plot.jpg",
       width = 10, height = 6, device='jpg', dpi=2000)

biome_level_NSE <-
  ggplot() +
  geom_sf(data = world, color = "black",lwd = 0.001)+
  geom_sf(data = biome_predictions_plot, color = "grey",lwd = 0.001,
          aes(fill = NSE))+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1)+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

biome_level_NSE
ggsave(biome_level_NSE, path = ".",
       filename = "/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/output/figures/NSE_biome_plot.jpg",
       width = 10, height = 6, device='jpg', dpi=2000)




######################################
### HEX LEVEL RANDOM FOREST MODELS ###
######################################

slope_data <- readRDS("/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/output/hylak_id_slopes.rds") %>%
  filter(sig_lake_change == "YES") %>%
  filter(lake_type == 1) %>%
  filter(elevation >= 0) %>%
  rename(`Lake Area Change` = fit_total_slope) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid <- st_make_grid(
  world,
  n = c(125, 125), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  square = FALSE) %>%
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid)

area_hexes <- st_join(slope_data, grid, join = st_intersects)

area_hexes_avg <- area_hexes %>%
  st_drop_geometry() %>%
  group_by(index, hybas_id) %>%
  summarise(shore_dev = mean(shore_dev, na.rm = TRUE),
            depth_avg = mean(depth_avg, na.rm = TRUE),
            res_time = mean(res_time, na.rm = TRUE),
            mk_total_p_val = median(mk_total_p_val, na.rm = TRUE),
            elevation = mean(elevation, na.rm = TRUE),
            slope_100 = mean(slope_100, na.rm = TRUE),
            `Lake Area Change` = mean(`Lake Area Change`, na.rm = TRUE),
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
  st_sf() %>%
  na.omit(.)

hex_rf <- list()
indexes <- unique(area_hexes_avg$index)

for(i in 1:length(indexes)){
  b <- area_hexes %>% filter(index == indexes[i]) %>%
    dplyr::do(model = randomForest::randomForest(formula = `Lake Area Change` ~
                                                   fit_precip_slope +
                                                   fit_snow_slope +
                                                   fit_temp_slope +
                                                   fit_pop_slope +
                                                   fit_humid_slope +
                                                   fit_lw_slope +
                                                   shore_dev +
                                                   elevation +
                                                   slope_100 +
                                                   wshd_area, data = ., na.action=na.roughfix)) %>%
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
  mutate(strong_NSE = ifelse(NSE>=0.25, "STRONG","WEAK"))%>%
  right_join(grid, by = "index")%>%
  st_sf()


lake_change_predictors <-
  ggplot() +
  geom_sf(data = hex_level_rf,lwd = 0.3,
          aes(fill = predictor, color = strong_NSE))+
  scale_fill_viridis(option = "F", na.value = "gray",
    direction = -1, discrete = T, name = "**Hybro Basin Predictor** <br> Random Forest")+
  scale_color_manual(values = c("blue", NA, NA),name = "**Predictive Skill** <br> NSE") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  #guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

lake_change_predictors
ggsave(lake_change_predictors, path = ".",
       filename = "/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/output/figures/HEX_level_predictors.jpg",
       width = 10, height = 6, device='jpg', dpi=2000)

