
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, glue, rgeos, stringr, SPEI, tidyverse, gtools, foreach, doSNOW, parallel)
rm(list = ls())
source('08_functions.R')

# Load data ---------------------------------------------------------------
fls <- list.files('../tif/climate/CHELSA/crn/bbox', full.names = TRUE, patter = '.tif')
vrs <- c('prec', 'tmax', 'temp', 'tmin')

# Apply the function to read the rasters ----------------------------------
for(i in 1:length(vrs)){ eval(parse(text = paste0(vrs[i], '<- readRaster(x = vrs[', i, '])')))}

# Download shapefile data -------------------------------------------------
ad2 <- rbind(raster::getData('GADM', country = 'COL', level = 1), raster::getData('GADM', country = 'ECU', level = 1), raster::getData('GADM', country = 'PER', level = 1))
ad2 <- raster::crop(ad2, extent(raster(fls[[1]]) * 0))
ad2@data$gid <- 1:nrow(ad2@data)

# Calculating the PET -----------------------------------------------------
cl <- makeCluster(28)
registerDoSNOW(cl)
rsl <- foreach(z = 1:nrow(ad2@data), .packages = c('raster', 'rgdal', 'glue', 'stringr', 'tidyverse', 'gtools'), .verbose = TRUE) %dopar% {
  calcPET(x = z)
}
stopCluster(cl)
