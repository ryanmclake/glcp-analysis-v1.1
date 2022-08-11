model_data <- vroom::vroom("./output/slopes/hybas_id_lm_residual_all.csv", delim = " ", col_names = T) %>%
  na.omit(.) %>%
  filter(lake_type == 1) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
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

area_hexes <- st_join(model_data, grid, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(res_std = mean(res_std, na.rm = TRUE)) %>%
  right_join(grid, by="index") %>%
  st_sf()


lake_area_residuals <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes,lwd = 0.05,
          aes(fill = res_std))+
  scale_fill_gradient(low="dodgerblue4",
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


ggsave(lake_area_residuals, path = ".",
       filename = "./output/figures/slope_global_plot_1_residuals.jpg",
       width = 14, height = 8, device='jpg', dpi=300)
