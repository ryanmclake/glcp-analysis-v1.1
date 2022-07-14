
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

  global_rf <- e
