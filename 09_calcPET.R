
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, glue, rgeos, stringr, SPEI, tidyverse, gtools, foreach, doSNOW, parallel)
rm(list = ls())
source('08_functions.R')

# Functions to use --------------------------------------------------------
readRaster <- function(x){
  stk <- grep(x, fls, value = TRUE) %>% 
    mixedsort() %>% 
    stack() 
}

# Load data ---------------------------------------------------------------
adm <- rbind(raster::getData('GADM', country = 'COL', level = 1), raster::getData('GADM', country = 'ECU', level = 1), raster::getData('GADM', country = 'PER', level = 1))
lim <- shapefile('../shp/base/mask.shp')

rad <- list.files('../tif/climate/SRAD/ET_SolRad/ET_SolRad', full.names = TRUE) %>% 
  grep(paste0('et_solrad_', 1:12, '$', collapse = '|'), ., value = TRUE) %>% 
  mixedsort() %>% 
  stack() %>% 
  raster::crop(., adm) %>% 
  raster::mask(., adm)

# CHELSA ------------------------------------------------------------------
fls <- list.files('../tif/climate/CHELSA/crn/bbox', full.names = TRUE)
tmx <- readRaster(x = 'tmax')
tav <- readRaster(x = 'tmean')
tmn <- readRaster(x = 'tmin')
ppt <- readRaster(x = 'prec')

# Mask and write the raster files, checking the extent
msk <- tmx[[1]] * 0
rad <- raster::resample(rad, msk)
rad <- raster::crop(rad, lim)
dir.create('../tif/climate/SRAD/countries')
Map('writeRaster', x = unstack(rad), filename = glue('../tif/climate/SRAD/countries/{names(ra2)}.tif'), overwrite = TRUE)

ETstack <- 0.0013*0.408*rad*(tmn+17)*(tmx-tmn-0.0123*ppt)^0.76
Map('writeRaster', x = unstack(ETstack), filename = glue('../tif/climate/CHELSA/crn/bbox/etp_{1:12}.tif'), overwrite = TRUE)

# Worldclim ----------------------------------------------------------------
fls <- list.files('../tif/climate/WORLDCLIM/crn', full.names = TRUE)
tmx <- readRaster(x = 'tmax') %>% raster::crop(., lim) %>% raster::mask(., lim)
tav <- readRaster(x = 'tmean') %>% raster::crop(., lim) %>% raster::mask(., lim)
tmn <- readRaster(x = 'tmin') %>% raster::crop(., lim) %>% raster::mask(., lim)
ppt <- readRaster(x = 'prec') %>% raster::crop(., lim) %>% raster::mask(., lim)

# Check the extent
ETstack <- 0.0013*0.408*rad*(tmn+17)*(tmx-tmn-0.0123*ppt)^0.76
dir.create('../tif/climate/WORLDCLIM/crn/bbox')
Map('writeRaster', x = unstack(ETstack), filename = glue('../tif/climate/WORLDCLIM/crn/bbox/etp_{1:12}.tif'), overwrite = TRUE)


