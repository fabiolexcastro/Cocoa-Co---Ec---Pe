
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, shadowtext, velox, sf, ggspatial, tidyverse, gtools, glue, foreach, doSNOW, parallel)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------
pnt <- read_csv('W:/_points/_cacao/_global/occ_cacao_april.csv')
zne <- shapefile('../shp/base/countries.shp')
all <- shapefile('../shp/base/all_countries.shp')
unique(pnt$Species) %>% sort()

sort(unique(pnt$Species))
sort(unique(pnt$Country))

# Filter for the study zone
sub <- pnt %>% 
  filter(Country %in% c('Colombia', 'Ecuador', 'Peru')) 
coordinates(sub) <- ~ Longitude + Latitude

dir.create('../shp/points')
shapefile(sub, '../shp/points/cacao.shp')

crd <- coordinates(sub) %>% 
  as.data.frame %>% 
  as_tibble

lbl <- coordinates(all) %>% 
  as.data.frame %>% 
  mutate(label = all@data$ENGLISH) %>% 
  as_tibble() %>% 
  setNames(c('Lon', 'Lat', 'Label')) %>% 
  filter(Label %in% c('Panama', 'Colombia', 'Ecuador', 'Peru', 'Venezuela'))

# To make the Map ---------------------------------------------------------
gg <- ggplot() +
  geom_point(data = crd, aes(x = Longitude, y = Latitude), col = 'brown', size = 0.8) +
  geom_sf(data = st_as_sf(zne), fill = NA, col = 'white') +
  geom_sf(data = st_as_sf(all), fill = NA) +
  coord_sf(xlim = c(-81, extent(zne)[2]), ylim = c(-16, 12)) +
  ggtitle(label = 'Cocoa presences (Colombia, Euador, Peru)') +
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line'),
        strip.background = element_blank(),
        strip.text = element_blank()) + 
  geom_text(data = lbl, aes(x = Lon, y = Lat, label = Label), size = 3.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
dir.create('../png/maps', recursive = TRUE)
ggsave(plot = gg, filename = '../png/maps/points_cocoa.png', units = 'in', width = 5, height = 9, dpi = 300)
