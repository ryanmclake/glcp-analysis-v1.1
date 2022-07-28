s = Sys.time()

library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

ds <-read_csv_arrow(
  "/data/katz/projects/glcp-analysis/data/glcp_extended.csv",
  quote = "\"",
  escape_double = TRUE,
  escape_backslash = FALSE,
  schema = NULL,
  col_names = TRUE,
  col_types = NULL,
  col_select = NULL,
  na = c("", "NA"),
  quoted_na = TRUE,
  skip_empty_rows = TRUE,
  skip = 0L,
  parse_options = NULL,
  convert_options = NULL,
  read_options = NULL,
  as_data_frame = TRUE,
  timestamp_parsers = NULL
)

system.time(ds %>%
              group_by(year, hylak_id, hybas_id, centr_lat, centr_lon, lake_type,
                       shore_dev, depth_avg, res_time,
                       elevation, slope_100, wshd_area) %>%
              collect() %>%
              summarize(total_km2 = mean(total_km2, na.rm = T),
                        total_precip_mm = sum(total_precip_mm, na.rm = T),
                        mean_temp_k = mean(mean_temp_k, na.rm = T),
                        pop_sum = mean(pop_sum, na.rm = T),
                        mean_spec_humidity = mean(mean_spec_humidity, na.rm = T),
                        mean_totcloud_pct = mean(mean_totcloud_pct, na.rm = T),
                        mean_sw_wm2 = mean(mean_sw_wm2, na.rm = T),
                        mean_lw_wm2 = mean(mean_lw_wm2, na.rm = T),
                        snow_km2 = mean(snow_km2, na.rm = T)) %>%
              ungroup(.) %>%
              mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
              mutate(across(c(13:21), ~ if_else(is.na(.),0,.))) %>%
              mutate(across(c(13:21), ~ scale(.))) %>%
              mutate(across(c(13:21), ~ if_else(is.nan(.),0,.)))%>%
              group_by(hylak_id, hybas_id, centr_lon, centr_lat, lake_type,
                              shore_dev, depth_avg, res_time,
                              elevation, slope_100, wshd_area) %>%
              do(total_km2 = lm(total_km2 ~ year, data = ., na.action = na.exclude),
                        precip_mm = lm(total_precip_mm ~ year, data = ., na.action = na.exclude),
                        snow_km2 = lm(snow_km2 ~ year, data = ., na.action = na.exclude),
                        mean_temp = lm(mean_temp_k ~ year, data = ., na.action = na.exclude),
                        pop_sum = lm(pop_sum ~ year, data = ., na.action = na.exclude),
                        spec_hum = lm(mean_spec_humidity ~ year, data = ., na.action = na.exclude),
                        total_cloud = lm(mean_totcloud_pct ~ year, data = ., na.action = na.exclude),
                        sw = lm(mean_sw_wm2 ~ year, data = ., na.action = na.exclude),
                        lw = lm(mean_lw_wm2 ~ year, data = ., na.action = na.exclude),
                        mk_total_km2 = MannKendall(.$total_km2)) %>%
              collect() %>%
              mutate(total_slope = .$total_km2[[1]]$coefficients[2],
                                                 total_rsq = summary(.$total_km2[[1]])$r.square,
                                                 mk_total_p_val = .$mk_total_km2[[1]]$sl[1],
                                                 precip_slope = .$precip_mm[[1]]$coefficients[2],
                                                 precip_rsq = summary(.$precip_mm[[1]])$r.square,
                                                 snow_slope = .$snow_km2[[1]]$coefficients[2],
                                                 snow_rsq = summary(.$snow_km2[[1]])$r.square,
                                                 temp_slope = .$mean_temp[[1]]$coefficients[2],
                                                 temp_rsq = summary(.$mean_temp[[1]])$r.square,
                                                 pop_slope = .$pop_sum[[1]]$coefficients[2],
                                                 pop_rsq = summary(.$pop_sum[[1]])$r.square,
                                                 humid_slope = .$spec_hum[[1]]$coefficients[2],
                                                 humid_rsq = summary(.$spec_hum[[1]])$r.square,
                                                 cloud_slope = .$total_cloud[[1]]$coefficients[2],
                                                 cloud_rsq = summary(.$total_cloud[[1]])$r.square,
                                                 sw_slope = .$sw[[1]]$coefficients[2],
                                                 sw_rsq = summary(.$sw[[1]])$r.square,
                                                 lw_slope = .$lw[[1]]$coefficients[2],
                                                 lw_rsq = summary(.$lw[[1]])$r.square)%>%
              select(-total_km2,
                            -mk_total_km2,
                            -precip_mm,
                            -snow_km2,
                            -mean_temp,
                            -pop_sum,
                            -spec_hum,
                            -total_cloud,
                            -sw,
                            -lw) %>%
              mutate(sig_lake_change = ifelse(mk_total_p_val<=0.05, "YES","NO")) %>%
              write.table(., file = paste0("/data/katz/projects/glcp-analysis/output/hylak_id_slopes2.csv"),
                          append = T,
                          row.names = F,
                          col.names = !file.exists("/data/katz/projects/glcp-analysis/output/hylak_id_slopes2.csv")))



e <- Sys.time()
t=e-s
print(t)
