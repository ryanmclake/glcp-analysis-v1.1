
# sites <- c(unique(EcoregionMask$BIOME))
# lake_to_biome <- list()
#
#   for(i in 1:length(sites)){
#      b <- EcoregionMask %>% filter(BIOME == sites[i])
#      b2 <- data %>%
#        cbind(b[st_nearest_feature(data, b),]) %>%
#        mutate(dist = st_distance(geometry, geometry.1, by_element = T))
#      b2$dist <- drop_units(b2$dist)
#      b2 <- b2 %>% filter(dist == 0) %>% arrange(hylak_id)
#      lake_to_biome[[i]] <- b2
#   }
#
# lake_to_biome = do.call(rbind, lake_to_biome)
#
# biome_change <- lake_to_biome %>%
#   dplyr::mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   filter(lake_type == 1)%>%
#   dplyr::mutate(biome_join = case_when(
#     BIOME == 1 ~ "TROPICAL",
#     BIOME == 2 ~ "TROPICAL",
#     BIOME == 3 ~ "TROPICAL",
#     BIOME == 4 ~ "TEMPERATE",
#     BIOME == 5 ~ "TEMPERATE",
#     BIOME == 6 ~ "BOREAL/TUNDRA",
#     BIOME == 7 ~ "TROPICAL",
#     BIOME == 8 ~ "TEMPERATE",
#     BIOME == 9 ~ "TEMPERATE",
#     BIOME == 10 ~ "TEMPERATE",
#     BIOME == 11 ~ "BOREAL/TUNDRA",
#     BIOME == 12 ~ "TEMPERATE",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "TROPICAL",
#     BIOME == 98 ~ "TEMPERATE",
#     BIOME == 99 ~ "BOREAL/TUNDRA",
#     TRUE ~ NA_character_))
#
# biome_change_plot <- ggplot(biome_change, aes(biome_join, fit_total_slope, group = biome_join))+
#   geom_boxplot(aes(fill = biome_join), color = "black", lwd = 0.6, alpha = 0.1, show.legend = FALSE) +
#   labs(x = "WWF Biomes", y = "Change in total lake area (km2)") +
#   stat_summary(
#     fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., color = "Mean"),
#     width = .75, linetype = "dashed"
#   ) +
#   stat_summary(
#     fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., color = "Median"),
#     width = .75, linetype = "solid"
#   ) +
#   scale_colour_manual("", values = c(Median = "dodgerblue4", Mean = "darkred")) +
#   scale_fill_manual(values = c("black", "darkorange3", "forestgreen", "magenta1")) +
#   theme_minimal() +
#   theme(text = element_text(size = 14),legend.position = "top",
#         panel.grid.major.y = element_line(c(0, 25, 50, 75, 100), color = "black",size = 0.2),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
#         axis.text.y = element_text(color = "black"))
#
# biome_change_plot
# ggsave(path = Dir.Base, filename = "./output/figures/WWF_biome_type_trends.jpg", width = 10, height = 8, device='jpg', dpi=700)
#
# biome_rf <- list()
# biomes <- unique(biome_change$biome_type)
#
# for(i in 1:length(biomes)){
#   b <- biome_change %>% filter(biome_type == biomes[i])%>%
#     dplyr::do(model = randomForest::randomForest(formula = fit_total_slope ~
#                                                    fit_precip_slope +
#                                                    fit_snow_slope +
#                                                    fit_temp_slope +
#                                                    fit_pop_slope +
#                                                    fit_humid_slope +
#                                                    fit_lw_slope +
#                                                    shore_dev +
#                                                    elevation +
#                                                    slope_100 +
#                                                    wshd_area, data = ., na.action=na.roughfix)) %>%
#     dplyr::collect() %>%
#     dplyr::ungroup(.)
#
#   e <- as.data.frame(b$model[[1]]$importance)
#   e$predictor <- row.names(e)
#   e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)
#   e$biome_type = biomes[i]
#
#   e <- e %>% arrange(-IncNodePurity)
#
#   biome_rf[[i]] <- e
#
# }
#
# biome_level_rf = do.call(rbind, biome_rf) %>%
#   group_by(biome_type) %>%
#   slice(1)
#
# shp <- read_sf(paste0("/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "BROADLEAF FOREST",
#     BIOME == 2 ~ "BROADLEAF FOREST",
#     BIOME == 3 ~ "CONIFEROUS FOREST",
#     BIOME == 4 ~ "CONIFEROUS FOREST",
#     BIOME == 5 ~ "BROADLEAF FOREST",
#     BIOME == 6 ~ "CONIFEROUS FOREST",
#     BIOME == 7 ~ "GRASSLAND",
#     BIOME == 8 ~ "GRASSLAND",
#     BIOME == 9 ~ "GRASSLAND",
#     BIOME == 10 ~ "GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "BROADLEAF FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext")
#
# world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
#   st_transform("+proj=eqearth +wktext")
#
# biome_predictions <- left_join(biome_change, biome_level_rf, by = "biome_type") %>%
#   st_transform("+proj=eqearth +wktext")
#
# biome_predictions_plot <-st_join(shp,biome_predictions, join = st_intersects)%>%
#   st_transform("+proj=eqearth +wktext")
#
# biome_level_driver <-
#   ggplot() +
#   geom_sf(data = world, color = "black",lwd = 0.001)+
#   geom_sf(data = biome_predictions_plot, color = "grey",lwd = 0.001,
#           aes(fill = predictor))+
#   scale_fill_viridis_d(
#     option = "magma", na.value = "gray",
#     direction = -1)+
#   coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
#   #guides(fill = guide_colourbar(title.position = "top"))+
#   theme_void()+
#   theme(legend.position = c(0.11, 0.35),
#         legend.direction = "vertical",
#         legend.title = ggtext::element_markdown(size = 10),
#         legend.text = element_text(size=9),
#         legend.key.height  = unit(.5, 'cm'),
#         legend.key.width =  unit(.3, 'cm'))
#
# biome_level_driver
# ggsave(biome_level_driver, path = ".",
#        filename = "/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/output/figures/predictor_biome_plot.jpg",
#        width = 10, height = 6, device='jpg', dpi=2000)
#
# biome_level_NSE <-
#   ggplot() +
#   geom_sf(data = world, color = "black",lwd = 0.001)+
#   geom_sf(data = biome_predictions_plot, color = "grey",lwd = 0.001,
#           aes(fill = NSE))+
#   scale_fill_viridis_c(
#     option = "magma", na.value = "gray",
#     direction = -1)+
#   coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
#   guides(fill = guide_colourbar(title.position = "top"))+
#   theme_void()+
#   theme(legend.position = c(0.11, 0.35),
#         legend.direction = "vertical",
#         legend.title = ggtext::element_markdown(size = 10),
#         legend.text = element_text(size=9),
#         legend.key.height  = unit(.5, 'cm'),
#         legend.key.width =  unit(.3, 'cm'))
#
# biome_level_NSE
# ggsave(biome_level_NSE, path = ".",
#        filename = "/Volumes/SeagateBackupPlusDrive/glcp-analysis-v1.1/output/figures/NSE_biome_plot.jpg",
#        width = 10, height = 6, device='jpg', dpi=2000)
