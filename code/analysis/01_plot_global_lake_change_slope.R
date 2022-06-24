library(sf)
library(dplyr)
library(ggplot2)
library(maps)
library(patchwork)
library(hexbin)
library(rnaturalearth)
library(units)
library(zoo)
library(randomForest)
library(viridis)
library(rpart)
library(rpart.plot)

vroom::vroom("./output/slopes/hylak_id_slopes.csv", delim = " ") %>%
  saveRDS(., file = "./output/slopes/hylak_id_slopes.rds")

slope_data <- readRDS("./output/slopes/hylak_id_slopes.rds") %>%
  rename(`Lake Area Change` = fit_total_slope) %>%
  filter(lake_type == 1) %>%
  filter(elevation >= 0) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid <- st_make_grid(
  world,
  n = c(200, 200), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  flat_topped = T,
  square = FALSE) %>%
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid)

area_hexes <- st_join(slope_data, grid, join = st_intersects)

area_hexes_avg <- area_hexes %>%
  st_drop_geometry() %>%
  group_by(index) %>%
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
            fit_lw_slope = mean(fit_lw_slope, na.rm = TRUE)) %>%
  mutate(sig_lake_change = ifelse(mk_total_p_val<=0.05, "YES","NO"))%>%
  right_join(grid, by="index") %>%
  st_sf()

lake_area_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.3,
    aes(fill = `Lake Area Change`, color = sig_lake_change))+
  geom_sf(data = shp_boreal,lwd = 0, color = "black", fill = "black", alpha = 0.1)+
  geom_sf(data = shp_desert,lwd = 0, color = "darkorange3", fill = "darkorange3", alpha = 0.1)+
  geom_sf(data = shp_temperate,lwd = 0, color = "forestgreen", fill = "forestgreen", alpha = 0.1)+
  geom_sf(data = shp_tropical,lwd = 0, color = "magenta1", fill = "magenta1", alpha = 0.1)+
  scale_fill_gradient2(midpoint=0, low="orange4", mid="white",
                       high="cyan", space ="Lab", na.value="white",
                       name = "**Standardized ΔLSA** <br>(km<sup>2</sup> yr<sup>-1</sup>)") +
  scale_color_manual(values = c(NA, "blue", NA), na.value = "white", name = "**Mann-Kendall test** <br>(p-value < 0.05)") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

lake_area_change

change <- lake_area_change + biome_change_plot

ggsave(change, path = ".",
       filename = "./output/figures/slope_global_plot_boxplots.jpg",
       width = 20, height = 8, device='jpg', dpi=1000)
