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
library(outliers)


# shp_boreal <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext") %>%
#   filter(biome_type %in% c("BOREAL FOREST","ROCK & ICE","TUNDRA"))
#
# shp_desert <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext") %>%
#   filter(biome_type == "DESERT")
#
#
# shp_temperate <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext") %>%
#   filter(biome_type %in% c("TEMPERATE GRASSLAND","TEMPERATE CONIFEROUS FOREST","MEDITERRANIAN FOREST","FLOODED GRASSLAND",
#                            "MONTANE GRASSLAND","LAKE","TEMPERATE BROADLEAF FOREST"))
#
# shp_tropical <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext") %>%
#   filter(biome_type %in% c("TROPICAL MOIST FOREST","TROPICAL DRY FOREST","TROPICAL GRASSLAND","MANGROVES",
#                            "TROPICAL CONIFEROUS FOREST"))


slope_data <- vroom::vroom("./output/slopes/hylak_id_slopes6.csv", delim = " ", col_names = T) %>%
  rename(lsa_pval = p.value...8,
         lsa_tau = statistic...9,
         precip_pval = p.value...10,
         precip_tau = statistic...11,
         temp_pval = p.value...12,
         temp_tau = statistic...13,
         pop_pval = p.value...14,
         pop_tau = statistic...15,
         snow_pval = p.value...16,
         snow_tau = statistic...17) %>%
  filter(lake_type == 1) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(lsa_tau <= 1) %>%
  na.omit(.)

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

area_hexes <- st_join(slope_data, grid, join = st_intersects)

# test <- area_hexes %>%
#   cbind(EcoregionMask_hex[st_nearest_feature(area_hexes, EcoregionMask_hex),]) %>%
#   mutate(dist = st_distance(geometry, geometry.1, by_element = T))
#
# test$dist <- drop_units(test$dist)
# test <- test %>% filter(dist == 0) %>% arrange(hylak_id)

area_hexes_avg <- area_hexes %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(depth_avg = median(depth_avg, na.rm = TRUE),
            elevation = median(elevation, na.rm = TRUE),
            lsa_pval = median(lsa_pval, na.rm = TRUE),
            lsa_tau = median(lsa_tau, na.rm = TRUE),
            precip_pval = median(precip_pval, na.rm = TRUE),
            precip_tau = median(precip_tau, na.rm = TRUE),
            temp_pval = median(temp_pval, na.rm = TRUE),
            temp_tau = median(temp_tau, na.rm = TRUE),
            pop_pval = median(pop_pval, na.rm = TRUE),
            pop_tau = median(pop_tau, na.rm = TRUE),
            snow_pval = median(snow_pval, na.rm = TRUE),
            snow_tau = median(snow_tau, na.rm = TRUE)) %>%
  right_join(grid, by="index") %>%
  st_sf()

lake_area_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
    aes(fill = lsa_tau))+
  scale_fill_gradient2(midpoint=0, low="orange1", mid="white",
                       high="cyan", space ="Lab", na.value="grey",
                       name = "**Δ Lake Surface Area** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(lake_area_change, path = ".",
       filename = "./output/figures/slope_global_plot_1_degree.jpg",
       width = 14, height = 8, device='jpg', dpi=300)


precip_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = precip_tau))+
  scale_fill_gradient2(midpoint=0, low="cyan", mid="white",
                       high="blue", space ="Lab", na.value="grey",
                       name = "**Δ Rainfall** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(precip_change, path = ".",
       filename = "./output/figures/slope_global_plot_1_degree_precip.jpg",
       width = 14, height = 8, device='jpg', dpi=300)


temp_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = temp_tau))+
  scale_fill_gradient2(midpoint=0, low="dodgerblue4", mid="white",
                       high="darkred", space ="Lab", na.value="grey",
                       name = "**Δ Temperature** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(temp_change, path = ".",
       filename = "./output/figures/slope_global_plot_1_degree_temp.jpg",
       width = 14, height = 8, device='jpg', dpi=300)


snow_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = snow_tau))+
  scale_fill_gradient2(midpoint=0, low="purple3", mid="white",
                       high="cyan", space ="Lab", na.value="grey",
                       name = "**Δ Snow** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(snow_change, path = ".",
       filename = "./output/figures/slope_global_plot_1_degree_snow.jpg",
       width = 14, height = 8, device='jpg', dpi=300)


pop_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = pop_tau))+
  scale_fill_gradient2(midpoint=0, low="green3", mid="white",
                       high="yellow3", space ="Lab", na.value="grey",
                       name = "**Δ Population** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

library(patchwork)

big_fig <- (lake_area_change + temp_change)/(precip_change + pop_change)

ggsave(big_fig, path = ".",
       filename = "./output/figures/slope_global_all_compare.jpg",
       width = 26, height = 14, device='jpg', dpi=300, limitsize = FALSE)
