
slope_data <- readRDS("./output/slopes/hylak_id_slopes.rds") %>%
  filter(sig_lake_change == "YES") %>%
  filter(lake_type == 1) %>%
  rename(`Lake Area Change` = fit_total_slope) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

b <- slope_data %>%
     dplyr::do(model = randomForest::randomForest(formula = `Lake Area Change` ~
                                                 fit_precip_slope +
                                                 fit_snow_slope +
                                                 fit_temp_slope +
                                                 fit_pop_slope +
                                                 fit_humid_slope +
                                                 fit_lw_slope+
                                                 #shore_dev +
                                                 elevation +
                                                 slope_100 +
                                                 wshd_area,
                                                 data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)

  e <- e %>% arrange(-IncNodePurity)

  global_rf <- e %>%
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
    mutate(IncNodePurity = round(IncNodePurity, digits = 0))


global_rf_figure <- ggplot(global_rf, aes(y = IncNodePurity, x = NA, group = predictor_new))+
                    geom_bar(aes(fill = predictor_new),stat = "identity")+
                    scale_fill_viridis(option = "C", na.value = "white",
                    direction = -1, discrete = T)+
                    geom_text(aes(label=IncNodePurity),color="black",size=5,
                              position=position_stack(vjust=0.5))
                    labs(title = "Global RF Node Purity")

