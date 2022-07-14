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
library(grid)

shp_boreal <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("BOREAL FOREST","ROCK & ICE","TUNDRA"))

shp_desert <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type == "DESERT")


shp_temperate <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("TEMPERATE GRASSLAND","TEMPERATE CONIFEROUS FOREST","MEDITERRANIAN FOREST","FLOODED GRASSLAND",
                           "MONTANE GRASSLAND","LAKE","TEMPERATE BROADLEAF FOREST"))

shp_tropical <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("TROPICAL MOIST FOREST","TROPICAL DRY FOREST","TROPICAL GRASSLAND","MANGROVES",
                           "TROPICAL CONIFEROUS FOREST"))


vroom::vroom("./output/slopes/hylak_id_slopes.csv", delim = " ") %>%
  saveRDS(., file = "./output/slopes/hylak_id_slopes.rds")

Dir.Base <- getwd()
Dir.Data <- file.path(Dir.Base, "data")
Dir.Shapes <- file.path(Dir.Data, "shapes")

if (!file.exists(file.path(Dir.Shapes, "WWF_ecoregions"))) {
  download.file("http://assets.worldwildlife.org/publications/15/files/original/official_teow.zip",
                destfile = file.path(Dir.Shapes, "wwf_ecoregions.zip"))
  unzip(file.path(Dir.Shapes, "wwf_ecoregions.zip"), exdir = file.path(Dir.Shapes, "WWF_ecoregions"))
}

data <- readRDS("./output/slopes/hylak_id_slopes.rds") %>%
  #filter(sig_lake_change == "YES") %>%
  filter(lake_type == 1) %>%
  filter(elevation >= 0) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326)

EcoregionMask <- read_sf(file.path(Dir.Base,"data","shapes", "WWF_ecoregions", "official", "wwf_terr_ecos.shp"))
EcoregionMask <- st_make_valid(EcoregionMask)

EcoregionMask_hex <- st_make_valid(EcoregionMask)%>%
  st_transform("+proj=eqearth +wktext")

slope_data <- readRDS("./output/slopes/hylak_id_slopes.rds") %>%
  rename(`Lake Area Change` = fit_total_slope) %>%
  #filter(lake_type == 1) %>%
  filter(elevation >= 0) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid_spacing <- 333000 # CRS units in meters (100000 m = 111 km & 111 km ~ 1 Decimal degree)

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

area_hexes <- st_join(slope_data, grid, join = st_intersects)

test <- area_hexes %>%
  cbind(EcoregionMask_hex[st_nearest_feature(area_hexes, EcoregionMask_hex),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))

test$dist <- drop_units(test$dist)
test <- test %>% filter(dist == 0) %>% arrange(hylak_id)

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
  geom_sf(data = area_hexes_avg,lwd = 0.6,
    aes(fill = `Lake Area Change`, color = sig_lake_change))+
  # geom_sf(data = shp_boreal,lwd = 0, color = "black", fill = "black", alpha = 0.1)+
  # geom_sf(data = shp_desert,lwd = 0, color = "firebrick4", fill = "firebrick4", alpha = 0.1)+
  # geom_sf(data = shp_temperate,lwd = 0, color = "forestgreen", fill = "forestgreen", alpha = 0.1)+
  # geom_sf(data = shp_tropical,lwd = 0, color = "magenta3", fill = "magenta3", alpha = 0.1)+
  scale_fill_gradient2(midpoint=0, low="orange1", mid="white",
                       high="cyan", space ="Lab", na.value="black",
                       name = "**Standardized ΔLSA** <br>(km<sup>2</sup> yr<sup>-1</sup>)") +
  scale_color_manual(values = c(NA, "blue", NA), na.value = "black", name = "**Mann-Kendall test** <br>(p-value < 0.05)") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

#change <- lake_area_change + (biome_change_plot+plot_spacer())

ggsave(lake_area_change, path = ".",
       filename = "./output/figures/slope_global_plot.jpg",
       width = 14, height = 8, device='jpg', dpi=2000)
