

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, gtools, sf, tidyverse, glue, stars, nngeo, SPEI, fields)
rm(list = ls())

# Functions to use  -------------------------------------------------------
readRaster <- function(x){
  stk <- grep(x, fls, value = TRUE) %>% 
    mixedsort() %>% 
    stack() 
}
rst2tbl <- function(x){
  rsl <- raster::as.data.frame(x, xy = TRUE, na.rm = TRUE) %>% 
    as_tibble() %>% 
    setNames(c('x', 'y', glue('{str_split(names(x), pattern = "_")[[1]][2]}_{month.abb}')))
}

# Load data ---------------------------------------------------------------
fls <- list.files('../tif/climate/CHELSA/crn', full.names = TRUE, pattern = '.tif$')
zne <- shapefile('../shp/base/countries.shp')
lim <- shapefile('../shp/base/mask.shp')
vrs <- readRDS(file = '../rds/vrs.rds')[2:5]

# Read rasters (stacks) ---------------------------------------------------
for(i in 1:length(vrs)){ eval(parse(text = paste0(vrs[i], '<- readRaster(x = vrs[', i, '])')))}
msk <- prec[[1]] * 0
vrs_tbl <- paste0(vrs, '_tbl')

prec_tbl <- rst2tbl(x = prec)
tmax_tbl <- rst2tbl(x = tmax)
temp_tbl <- rst2tbl(x = temp)
tmin_tbl <- rst2tbl(x = tmin)

# Example with the first pixel
prec1 <- prec_tbl[1,] %>% 
  gather(., var, value, -x, -y) %>% 
  mutate(month = str_sub(var, 6, nchar(var))) %>% 
  dplyr::select(x, y, month, prec = value)
tmax1 <- tmax_tbl[1,] %>% 
  gather(., var, value, -x, -y) %>% 
  mutate(month = str_sub(var, 8, nchar(var))) %>% 
  dplyr::select(x, y, month, tmax = value)
temp1 <- temp_tbl[1,] %>% 
  gather(., var, value, -x, -y) %>% 
  mutate(month = str_sub(var, 8, nchar(var))) %>% 
  dplyr::select(x, y, month, temp = value)
tmin1 <- tmin_tbl[1,] %>% 
  gather(., var, value, -x, -y) %>% 
  mutate(month = str_sub(var, 8, nchar(var))) %>% 
  dplyr::select(x, y, month, tmin = value)

data <- list(prec1, tmax1, temp1, tmin1) %>% purrr::reduce(left_join, by = c('x','y', 'month'))
har <- hargreaves(Tmin = pull(data, tmin),
                  Tmax = pull(data, tmax),
                  Pre = pull(data, prec),
                  lat = unique(data$y))


# 
# lat.val <- coordinates(msk)[,2]
# lat <- setValues(x = msk, values = lat.val)
# lat <- raster::crop(lat, lim) %>% raster::mask(., lim)
# 
# tst <- hargreaves(Tmin = tmin[[1]],
#                   Tmax = tmax[[1]],
#                   Pre = prec[[1]],
#                   lat = lat)
# 
# pet <- SPEI::hargreaves(Tmin = pull(as.data.frame(tmin, na.rm = TRUE)), 
#                         Tmax = pull(as.data.frame(tmax, na.rm = TRUE)),
#                         Pre = pull(as.data.frame(prec, na.rm = TRUE)),
#                         lat = pull(as.data.frame(lat, na.rm = TRUE)),
#                         na.rm = TRUE)
# data(wichita)
# attach(wichita)
# names(wichita)
# har <- hargreaves(TMIN,TMAX,lat=37.6475)
# # Penman, based on sun hours, ignore NAs
# pen <- penman(TMIN,TMAX,AWND,tsun=TSUN,lat=37.6475,z=402.6,na.rm=TRUE)
# # Penman, based on cloud cover
# pen2 <- penman(TMIN,TMAX,AWND,CC=ACSH,lat=37.6475,z=402.6,na.rm=TRUE)
# # Plot them together
# plot(cbind(tho,har,pen,pen2))
# # Now consider the data started i
# 

