s = Sys.time()

library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(doParallel, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)

country <- list.files(path = "./output/slopes/countries")
country <- gsub("\\..*", "", country)

analysis_function <- function(x){

    system.time(read_csv_arrow(
      paste0("./output/slopes/countries/",x,".csv"),
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
        group_by(year, hylak_id, hybas_id, centr_lat, centr_lon, lake_type, depth_avg, elevation) %>%
        collect() %>%
              summarize(total_km2 = mean(total_km2, na.rm = T),
                        total_precip_mm = sum(total_precip_mm, na.rm = T),
                        mean_temp_k = mean(mean_temp_k, na.rm = T),
                        pop_sum = mean(pop_sum, na.rm = T),
                        mean_spec_humidity = mean(mean_spec_humidity, na.rm = T),
                        mean_totcloud_pct = mean(mean_totcloud_pct, na.rm = T),
                        mean_sw_wm2 = mean(mean_sw_wm2, na.rm = T),
                        mean_lw_wm2 = mean(mean_lw_wm2, na.rm = T),
                        snow_km2 = mean(snow_km2, na.rm = T),
                        ice_cover_mean = mean(ice_cover_mean, na.rm = T)) %>%
        ungroup(.)%>%
        group_by(hylak_id) %>%
        mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
        mutate(area_lag = lag(total_km2)) %>%
        ungroup(.)%>%
        mutate(across(c(9:18), ~ scale(.))) %>%
        mutate(across(c(9:18), ~ if_else(is.na(.),0,.))) %>%
        na.omit(.) %>%
        group_by(hylak_id, hybas_id, centr_lat, centr_lon, lake_type) %>%
        do(mod = lm(total_km2 ~ area_lag + total_precip_mm + mean_temp_k + snow_km2 + ice_cover_mean + pop_sum, data = ., na.action = na.exclude)) %>%
        mutate(res_std = summary(mod)$sigma) %>%
        select(-mod) %>%
        collect()%>%
              write.table(., file = paste0("./output/slopes/hybas_id_lm_residual_all.csv"),
                          append = T,
                          row.names = F,
                          col.names = !file.exists("./output/slopes/hybas_id_lm_residual_all.csv")))
}


no_cores <- detectCores()-1
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(i=country) %dopar% analysis_function(i)

e <- Sys.time()
t=e-s
print(t)
