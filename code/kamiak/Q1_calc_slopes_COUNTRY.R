s = Sys.time()

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}

library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(doParallel, warn.conflicts = FALSE)
library(Kendall)

country <- list.files(path = "./output/slopes/countries")
country <- gsub("\\..*", "", country)

analysis_function <- function(x){

system.time(d1 <- read_csv_arrow(
  paste0("./output/slopes/countries/",country[1],".csv"),
  quote = "\"",
  escape_double = TRUE,
  escape_backslash = FALSE,
  schema = NULL,
  col_names = F,
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
  timestamp_parsers = NULL) %>% rename(year = f0,
                                       month=f1,
                                       hylak_id=f2,
                                       centr_lat=f3,
                                       centr_lon=f4,
                                       continent=f5,
                                       country=f6,
                                       bsn_lvl=f7,
                                       hybas_id=f8,
                                       mean_monthly_precip_mm=f9,
                                       total_precip_mm=f10,
                                       mean_annual_temp_k=f11,
                                       pop_sum=f12,
                                       seasonal_km2=f13,
                                       permanent_km2=f14,
                                       total_km2=f15,
                                       lake_name=f16,
                                       lake_type=f17,
                                       lake_area=f18,
                                       shore_dev=f19,
                                       vol_total=f20,
                                       vol_res=f21,
                                       vol_src=f22,
                                       depth_avg=f23,
                                       res_time=f24,
                                       elevation=f25,
                                       slope_100=f26,
                                       wshd_area=f27,
                                       pour_long=f28,
                                       pour_lat=f29,
                                       sub_area=f30,
                                       mean_spec_humidity=f31,
                                       mean_precip_mm=f32,
                                       sum_precip_mm=f33,
                                       mean_temp_k=f34,
                                       mean_totcloud_pct=f35,
                                       mean_sw_wm2=f36,
                                       mean_lw_wm2=f37,
                                       above_ratio_cutoff=f38,
                                       ice_cover_min=f39,
                                       ice_cover_max=f40,
                                       ice_cover_mean=f41,
                                       ice_cover_median=f42,
                                       ice_cover_binary_min=f43,
                                       ice_cover_binary_max=f44,
                                       ice_cover_binary_mean=f45,
                                       ice_cover_binary_median=f46,
                                       ice_cover_count=f47,
                                       snow_km2=f48) %>%
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
              ungroup(.)%>%
              group_by(hylak_id) %>%
              arrange(hylak_id) %>%
              mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
              filter(year >= "2000") %>%
              mutate(across(c(12:20), ~ if_else(is.nan(.),0,.))) %>%
              mutate(across(c(12:20), ~ scale(.))) %>%
              mutate(across(c(12:20), ~ if_else(is.nan(.),0,.))) %>%
              mutate(area_slope = slope(year,total_km2),
                     precip_slope = slope(year,total_precip_mm),
                     snow_slope = slope(year,snow_km2),
                     temp_slope = slope(year,mean_temp_k),
                     population_slope = slope(year,pop_sum),
                     humidity_slope = slope(year,mean_spec_humidity),
                     cloud_slope = slope(year,mean_totcloud_pct),
                     mk_p_val = MannKendall(total_km2)[['sl']][1]) %>%
              select(-total_km2,-mean_temp_k,-total_precip_mm,-snow_km2,-pop_sum,
                     -mean_spec_humidity,-mean_totcloud_pct,-year,-mean_lw_wm2,-mean_sw_wm2)%>%
              summarize_all(funs(mean)) %>%
              mutate(sig_lake_change = ifelse(mk_p_val<=0.05, "YES","NO")) %>%
              write.table(., file = paste0("./output/hylak_id_slopes5.csv"),
                          append = T,
                          row.names = F,
                          col.names = !file.exists("./output/hylak_id_slopes5.csv")))
}


no_cores <- detectCores()-1
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(i=country) %dopar% analysis_function(i)

e <- Sys.time()
t=e-s
print(t)
