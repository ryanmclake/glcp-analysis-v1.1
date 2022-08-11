model_data <- vroom::vroom("./output/slopes/hybas_id_lm1.csv", delim = " ", col_names = F) %>%
  rename(hybas_id = X1, centr_lat = X2, centr_lon = X3, elevation = X4, lake_type = X5,
         term = X6, estimate = X7, std_err = X8, statistic = X9, p_value = X10) %>%
  mutate(across(c(1:5,7:10), ~ as.numeric(.))) %>%
  filter(lake_type == 1) %>%
  na.omit(.) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

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

world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid_spacing <- 111000 # CRS units in meters (100000 m = 111 km & 111 km ~ 1 Decimal degree)

grid <- st_make_grid(
  world,
  cellsize = c(grid_spacing, grid_spacing),
  #n = c(200, 200), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  flat_topped = T,
  square = F) %>%
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid)

area_hexes <- st_join(model_data, grid, join = st_intersects)

area_hexes_2 <- area_hexes %>%
  st_drop_geometry() %>%
  group_by(hybas_id, index) %>%
  filter(estimate == min(estimate)) %>%
  group_by(index) %>%
  filter(estimate == min(estimate)) %>%
  right_join(grid, by="index") %>%
  st_sf() %>%
  mutate(term_2 = case_when(
        term == "area_lag" ~ "Lag + Intercept",
        term == "(Intercept)" ~ "Lag + Intercept",
        term == "ice_cover_mean" ~ "Lake Ice Cover",
        term == "total_precip_mm" ~ "Rainfall",
        term == "mean_temp_k" ~ "Temperature",
        term == "pop_sum" ~ "Population",
        term == "snow_km2" ~ "Snow Cover",
        term == NA ~ "Lag + Intercept",
        TRUE ~ NA_character_))



# area_hexes_avg <- area_hexes %>%
#   st_drop_geometry() %>%
#   group_by(index) %>%
#   summarise(depth_avg = median(depth_avg, na.rm = TRUE),
#             elevation = median(elevation, na.rm = TRUE),
#             lsa_pval = median(lsa_pval, na.rm = TRUE),
#             lsa_tau = median(lsa_tau, na.rm = TRUE),
#             precip_pval = median(precip_pval, na.rm = TRUE),
#             precip_tau = median(precip_tau, na.rm = TRUE),
#             temp_pval = median(temp_pval, na.rm = TRUE),
#             temp_tau = median(temp_tau, na.rm = TRUE),
#             pop_pval = median(pop_pval, na.rm = TRUE),
#             pop_tau = median(pop_tau, na.rm = TRUE),
#             snow_pval = median(snow_pval, na.rm = TRUE),
#             snow_tau = median(snow_tau, na.rm = TRUE)) %>%
#   right_join(grid, by="index") %>%
#   st_sf()

lake_change_predictors <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_2,lwd = 0.05,
          aes(fill = term_2))+
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T, name = "**Hybro Basin Predictor** <br> Multiple Regression")+
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


ggsave(lake_change_predictors, path = ".",
       filename = "./output/figures/slope_global_plot_1_degree_predictor.jpg",
       width = 14, height = 8, device='jpg', dpi=300)
