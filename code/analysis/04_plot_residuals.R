library(tidyverse)
library(sf)

model_data <- vroom::vroom("./output/slopes/hybas_id_lm_residual_all.csv", delim = " ", col_names = T) %>%
  na.omit(.) %>%
  filter(lake_type == 1) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

model_pop <- vroom::vroom("./output/slopes/hybas_id_lm_residual_pop_only.csv", delim = " ", col_names = T) %>%
  na.omit(.) %>%
  filter(lake_type == 1)

model_clim <- vroom::vroom("./output/slopes/hybas_id_lm_residual_no_pop.csv", delim = " ", col_names = T) %>%
  na.omit(.) %>%
  filter(lake_type == 1)

model_none <- vroom::vroom("./output/slopes/hybas_id_lm_residual_pop_none.csv", delim = " ", col_names = T) %>%
  na.omit(.) %>%
  filter(lake_type == 1)

build <- dplyr::left_join(model_data, model_pop, by = c("hylak_id", "hybas_id", "lake_type", "centr_lat", "centr_lon")) %>%
  dplyr::left_join(., model_clim, by = c("hylak_id", "hybas_id", "lake_type", "centr_lat", "centr_lon")) %>%
  dplyr::left_join(., model_none, by = c("hylak_id", "hybas_id", "lake_type", "centr_lat", "centr_lon")) %>%
  rename(rss_all = res_std.x,
         rss_pop = res_std.y,
         rss_clim = res_std.x.x,
         rss_lag = res_std.y.y)%>%
  mutate(rss_dif_clim = rss_clim-rss_lag,
         rss_dif_pop = rss_pop-rss_lag,
         rss_dif_all = rss_all-rss_lag,
         rss_dif_lag = rss_lag-rss_lag) %>%
  select(-rss_all, -rss_pop, -rss_clim, -rss_lag) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")


world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid_spacing <- 444000 # CRS units in meters (100000 m = 111 km & 111 km ~ 1 Decimal degree)

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

area_hexes_all <- st_join(model_data, grid, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(res_std = median(res_std, na.rm = TRUE)) %>%
  right_join(grid, by="index") %>%
  st_sf()

area_hexes_pop <- st_join(model_pop, grid, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(res_std = median(res_std, na.rm = TRUE)) %>%
  mutate(res_std = sqrt(res_std)) %>%
  right_join(grid, by="index") %>%
  st_sf()

area_hexes_clim <- st_join(model_clim, grid, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(res_std = median(res_std, na.rm = TRUE)) %>%
  mutate(res_std = sqrt(res_std)) %>%
  right_join(grid, by="index") %>%
  st_sf()

area_hexes_none <- st_join(model_none, grid, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(res_std = median(res_std, na.rm = TRUE)) %>%
  mutate(res_std = sqrt(res_std)) %>%
  right_join(grid, by="index") %>%
  st_sf()


lake_area_residuals_all <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_all,lwd = 0.05,
          aes(fill = rss_dif_clim))+
  scale_fill_gradient(low="white",
                       high="darkred", space ="Lab", na.value="grey",
                       name = "**Residual Std. Err.** <br>Climate + Population") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


lake_area_residuals_pop <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_pop,lwd = 0.05,
          aes(fill = res_std))+
  scale_fill_gradient(low="white",
                      high="darkred", space ="Lab", na.value="grey",
                      name = "**Residual Std. Err.** <br>Climate + Population") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

lake_area_residuals_clim <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_clim,lwd = 0.05,
          aes(fill = res_std))+
  scale_fill_gradient(low="white",
                      high="darkred", space ="Lab", na.value="grey",
                      name = "**Residual Std. Err.** <br>Climate + Population") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


lake_area_residuals_none <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_none,lwd = 0.05,
          aes(fill = res_std))+
  scale_fill_gradient(low="white",
                      high="darkred", space ="Lab", na.value="grey",
                      name = "**Residual Std. Err.** <br>Climate + Population") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))



ggsave(lake_area_residuals_all, path = ".",
       filename = "./output/figures/slope_global_plot_1_residuals.jpg",
       width = 14, height = 8, device='jpg', dpi=300)

ggsave(lake_area_residuals_pop, path = ".",
       filename = "./output/figures/slope_global_plot_1_residuals_pop.jpg",
       width = 14, height = 8, device='jpg', dpi=300)

ggsave(lake_area_residuals_clim, path = ".",
       filename = "./output/figures/slope_global_plot_1_residuals_clim.jpg",
       width = 14, height = 8, device='jpg', dpi=300)

ggsave(lake_area_residuals_none, path = ".",
       filename = "./output/figures/slope_global_plot_1_residuals_none.jpg",
       width = 14, height = 8, device='jpg', dpi=300)
