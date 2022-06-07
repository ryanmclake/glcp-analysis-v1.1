library(DBI)
library(RSQLite)
library(imputeTS)
library(tidyverse)
library(parallel)
library(dplyr)
library(Kendall)

run_lakes <- vroom::vroom("/Volumes/SeagateBackupPlusDrive/glcp-analysis/output/hylak_id_slopes.csv", delim = " ") %>%
  select(hylak_id)%>%
  t(.) %>%
  c(.)

lakes <- vroom::vroom("/Volumes/SeagateBackupPlusDrive/glcp-analysis/data/hylak_hybas_names.csv")%>%
  select(hylak_id)%>%
  filter(!hylak_id %in% c(run_lakes))%>%
  t(.) %>%
  c(.) %>%
  factor(.)

con <- dbConnect(RSQLite::SQLite(), dbname = "/Volumes/SeagateBackupPlusDrive/glcp-analysis/data/glcp_extended_SQLdb_small.sqlite")
dbDisconnect(con)


slope_function <- function(x){

  con <- dbConnect(RSQLite::SQLite(), dbname = "/Volumes/SeagateBackupPlusDrive/glcp-analysis/data/glcp_extended_SQLdb_small.sqlite")

  d <- DBI::dbGetQuery(con, paste0("SELECT * FROM GLCP_SQLdb WHERE hylak_id == ",x,""))

  dbDisconnect(con)

    d %>% group_by(year, hylak_id, hybas_id) %>%
    dplyr::summarize_all(funs(mean), na.rm = T) %>%
    dplyr::arrange(year) %>%
    ungroup(.) %>%
    #dplyr::filter(year > "2000")%>%
    dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
    dplyr::mutate(total_km2 = ifelse(is.nan(total_km2),0,total_km2),
                  total_precip_mm = ifelse(is.nan(total_precip_mm),0,total_precip_mm),
                  snow_km2 = ifelse(is.nan(snow_km2),0,snow_km2),
                  mean_temp_k = ifelse(is.nan(mean_temp_k),0,mean_temp_k),
                  pop_sum = ifelse(is.nan(pop_sum),0,pop_sum),
                  mean_spec_humidity = ifelse(is.nan(mean_spec_humidity),0,mean_spec_humidity),
                  mean_totcloud_pct = ifelse(is.nan(mean_totcloud_pct),0,mean_totcloud_pct),
                  mean_sw_wm2 = ifelse(is.nan(mean_sw_wm2),0,mean_sw_wm2),
                  mean_lw_wm2 = ifelse(is.nan(mean_lw_wm2),0,mean_lw_wm2)) %>%
    dplyr::mutate(scaled_total_km2 = scale(total_km2),
                  scaled_total_precip_mm = scale(total_precip_mm),
                  scaled_snow_km2 = scale(snow_km2),
                  scaled_mean_temp_k = scale(mean_temp_k),
                  scaled_pop_sum = scale(pop_sum),
                  scaled_mean_spec_humidity = scale(mean_spec_humidity),
                  scaled_mean_totcloud_pct = scale(mean_totcloud_pct),
                  scaled_mean_sw_wm2 = scale(mean_sw_wm2),
                  scaled_mean_lw_wm2 = scale(mean_lw_wm2)) %>%
    dplyr::mutate(scaled_total_km2 = ifelse(is.nan(scaled_total_km2),0,scaled_total_km2),
                  scaled_total_precip_mm = ifelse(is.nan(scaled_total_precip_mm),0,scaled_total_precip_mm),
                  scaled_snow_km2 = ifelse(is.nan(scaled_snow_km2),0,scaled_snow_km2),
                  scaled_mean_temp_k = ifelse(is.nan(scaled_mean_temp_k),0,scaled_mean_temp_k),
                  scaled_pop_sum = ifelse(is.nan(scaled_pop_sum),0,scaled_pop_sum),
                  scaled_mean_spec_humidity = ifelse(is.nan(scaled_mean_spec_humidity),0,scaled_mean_spec_humidity),
                  scaled_mean_totcloud_pct = ifelse(is.nan(scaled_mean_totcloud_pct),0,scaled_mean_totcloud_pct),
                  scaled_mean_sw_wm2 = ifelse(is.nan(scaled_mean_sw_wm2),0,scaled_mean_sw_wm2),
                  scaled_mean_lw_wm2 = ifelse(is.nan(scaled_mean_lw_wm2),0,scaled_mean_lw_wm2),
                  scaled_total_km2 = ifelse(is.infinite(scaled_total_km2),0,scaled_total_km2),
                  scaled_total_precip_mm = ifelse(is.infinite(scaled_total_precip_mm),0,scaled_total_precip_mm),
                  scaled_snow_km2 = ifelse(is.infinite(scaled_snow_km2),0,scaled_snow_km2),
                  scaled_mean_temp_k = ifelse(is.infinite(scaled_mean_temp_k),0,scaled_mean_temp_k),
                  scaled_pop_sum = ifelse(is.infinite(scaled_pop_sum),0,scaled_pop_sum),
                  scaled_mean_spec_humidity = ifelse(is.infinite(scaled_mean_spec_humidity),0,scaled_mean_spec_humidity),
                  scaled_mean_totcloud_pct = ifelse(is.infinite(scaled_mean_totcloud_pct),0,scaled_mean_totcloud_pct),
                  scaled_mean_sw_wm2 = ifelse(is.infinite(scaled_mean_sw_wm2),0,scaled_mean_sw_wm2),
                  scaled_mean_lw_wm2 = ifelse(is.infinite(scaled_mean_lw_wm2),0,scaled_mean_lw_wm2))%>%
    dplyr::group_by(hylak_id, hybas_id, pour_long, pour_lat, lake_type,
                    lake_area, shore_dev, vol_total, depth_avg, res_time,
                    elevation, slope_100, wshd_area) %>%
    dplyr::do(fit_total_km2 = lm(scaled_total_km2 ~ year, data = ., na.action = na.exclude),
              fit_precip_mm = lm(scaled_total_precip_mm ~ year, data = ., na.action = na.exclude),
              fit_snow_km2 = lm(scaled_snow_km2 ~ year, data = ., na.action = na.exclude),
              fit_mean_temp = lm(scaled_mean_temp_k ~ year, data = ., na.action = na.exclude),
              fit_pop_sum = lm(scaled_pop_sum ~ year, data = ., na.action = na.exclude),
              fit_spec_hum = lm(scaled_mean_spec_humidity ~ year, data = ., na.action = na.exclude),
              fit_total_cloud = lm(scaled_mean_totcloud_pct ~ year, data = ., na.action = na.exclude),
              fit_sw = lm(scaled_mean_sw_wm2 ~ year, data = ., na.action = na.exclude),
              fit_lw = lm(scaled_mean_lw_wm2 ~ year, data = ., na.action = na.exclude),
              mk_total_km2 = MannKendall(.$scaled_total_km2)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(fit_total_slope = .$fit_total_km2[[1]]$coefficients[2],
                  fit_total_rsq = summary(.$fit_total_km2[[1]])$r.square,
                  mk_total_p_val = .$mk_total_km2[[1]]$sl[1],
                  mk_total_tau = .$mk_total_km2[[1]]$tau[1],
                  fit_precip_slope = .$fit_precip_mm[[1]]$coefficients[2],
                  fit_precip_rsq = summary(.$fit_precip_mm[[1]])$r.square,
                  fit_snow_slope = .$fit_snow_km2[[1]]$coefficients[2],
                  fit_snow_rsq = summary(.$fit_snow_km2[[1]])$r.square,
                  fit_temp_slope = .$fit_mean_temp[[1]]$coefficients[2],
                  fit_temp_rsq = summary(.$fit_mean_temp[[1]])$r.square,
                  fit_pop_slope = .$fit_pop_sum[[1]]$coefficients[2],
                  fit_pop_rsq = summary(.$fit_pop_sum[[1]])$r.square,
                  fit_humid_slope = .$fit_spec_hum[[1]]$coefficients[2],
                  fit_humid_rsq = summary(.$fit_spec_hum[[1]])$r.square,
                  fit_cloud_slope = .$fit_total_cloud[[1]]$coefficients[2],
                  fit_cloud_rsq = summary(.$fit_total_cloud[[1]])$r.square,
                  fit_sw_slope = .$fit_sw[[1]]$coefficients[2],
                  fit_sw_rsq = summary(.$fit_sw[[1]])$r.square,
                  fit_lw_slope = .$fit_lw[[1]]$coefficients[2],
                  fit_lw_rsq = summary(.$fit_lw[[1]])$r.square) %>%
    dplyr::select(-fit_total_km2,
                  -mk_total_km2,
                  -fit_precip_mm,
                  -fit_snow_km2,
                  -fit_mean_temp,
                  -fit_pop_sum,
                  -fit_spec_hum,
                  -fit_total_cloud,
                  -fit_sw,
                  -fit_lw) %>%
    dplyr::mutate(sig_lake_change = ifelse(mk_total_p_val<=0.05, "YES","NO"),
                  trend_match = ifelse(mk_total_tau<0 & fit_total_slope>0, "MIS_MATCH","MATCH"),
                  trend_match = ifelse(mk_total_tau>0 & fit_total_slope<0, "MIS_MATCH","MATCH"))%>%
    write.table(., file = paste0("/Volumes/SeagateBackupPlusDrive/glcp-analysis/output/hylak_id_slopes.csv"),
                append = T,
                row.names = F,
                col.names = !file.exists("/Volumes/SeagateBackupPlusDrive/glcp-analysis/output/hylak_id_slopes.csv"))

  return(unique(d$hylak_id))

}

n.cores <- detectCores()-2

cl <- makeCluster(n.cores)

clusterExport(cl, list("%>%","collect", "vars","con","dbConnect","funs",
                       "dbDisconnect","ungroup","group_by","arrange","filter","MannKendall"))

parLapply(cl, lakes, slope_function)

stopCluster(cl)

vroom::vroom("/Volumes/SeagateBackupPlusDrive/glcp-analysis/output/hylak_id_slopes.csv", delim = " ") %>% saveRDS(., file = "/Volumes/SeagateBackupPlusDrive/glcp-analysis/output/hylak_id_slopes.rds")
