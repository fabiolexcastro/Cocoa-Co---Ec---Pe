
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, glue, foreach, doSNOW, parallel)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
extractMask <- function(gc, yr){
  x <- glue('{pth.ftr}/{gc}/r1i1p1/{yr}') %>% 
    list.files(., full.names = TRUE) %>% 
    grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>% 
    mixedsort() %>% 
    stack() %>% 
    raster::crop(., zne) %>% 
    raster::mask(., zne)
  glue('../tif/climate/ftr/{yr}/{gc}') %>% 
    dir.create(., recursive = TRUE)
  Map('writeRaster', x = unstack(x), filename = glue('../tif/climate/ftr/{yr}/{gc}/{names(x)}.asc'), overwrite = TRUE)
  print('Done!')
}

# Load data ---------------------------------------------------------------
pth.crn <- '//dapadfs/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2'
pth.ftr <- '//dapadfs/data_cluster_2/gcm/cmip5/downscaled/rcp60/global_30s'

# Download administrative areas
col <- raster::getData('GADM', country = 'COL', level = 0)
per <- raster::getData('GADM', country = 'PER', level = 0)
ecu <- raster::getData('GADM', country = 'ECU', level = 0)
zne <- rbind(col, per, ecu)

dir.create('../shp/base', recursive = TRUE)
shapefile(zne, '../shp/base/countries.shp')

# Current climate
crn <- list.files(pth.crn, full.names = TRUE, pattern = '.tif$') %>% 
  mixedsort() %>% 
  grep(paste0(c('bio', 'prec', 'tm'), collapse = '|'), ., value = TRUE) %>% 
  stack()
crn <- crn %>% 
  raster::crop(., zne) %>% 
  raster::mask(., zne)
dir.create('../tif/climate/crn', recursive = TRUE)
Map('writeRaster', x = unstack(crn), filename = glue('../tif/climate/crn/{names(crn)}.asc'))

# Future climate
gcm <- list.files(pth.ftr)
yrs <- c('2020_2049', '2040_2069')
vrs <- paste0(c(paste0('bio_', 1:19), paste0('tmax_', 1:12), paste0('tmin_', 1:12), paste0('tmean_', 1:12)), '$')

cl <- makeCluster(4)
registerDoSNOW(cl)

foreach(i = 1:length(gcm), .packages = c('raster', 'rgdal', 'rgeos', 'stringr', 'tidyverse', 'gtools', 'glue'), .verbose = TRUE) %dopar% {
  pth.ftr <- '//dapadfs/data_cluster_2/gcm/cmip5/downscaled/rcp60/global_30s'
  extractMask(gc = gcm[i], yr = yrs[2])
}
stopCluster(cl)



