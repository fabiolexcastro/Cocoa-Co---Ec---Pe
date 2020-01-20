
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, glue, rgeos, stringr, SPEI, tidyverse, gtools, tictoc)

# Functions to use --------------------------------------------------------
readRaster <- function(x){
  stk <- grep(x, fls, value = TRUE) %>% 
    mixedsort() %>% 
    stack() 
}
rst2tbl <- function(lyr, lim){
  nme <- names(lyr)[1] %>% 
    str_sub(., start = 8, end = 11)
  rsl <- lyr %>% 
    raster::crop(., lim) %>% 
    raster::mask(., lim) %>% 
    rasterToPoints(., xy = TRUE) %>% 
    as_tibble() %>%
    dplyr::select(x, y, everything()) %>% 
    setNames(c('x', 'y', glue('{nme}_{month.abb}'))) %>% 
    mutate(id = 1:nrow(.)) %>% 
    gather(var, value, -x, -y, -id) %>% 
    separate(col = var, into = c('variable', 'month'), sep = '_')
  rsl <- rsl[,c(1, 2, 3, 5, 6)] 
  colnames(rsl) <- c('x', 'y', 'id', 'month', nme)
  return(rsl)
}
calcPET <- function(x){
  # Extract the polygon
  zne <- ad2[ad2@data$gid == x,]
  # Cut the raster files
  pre <- rst2tbl(lyr = prec, lim = zne)
  tmx <- rst2tbl(lyr = tmax, lim = zne)
  tav <- rst2tbl(lyr = temp, lim = zne)
  tmn <- rst2tbl(lyr = tmin, lim = zne)
  # All the tables into only one
  tbl <- list(pre, tmx, tav, tmn) %>% 
    purrr::reduce(left_join, by = c('x', 'y', 'id', 'month')) %>% 
    mutate(tmax = tmax / 10, tmin = tmin / 10, temp = temp / 10)
  saveRDS(object = tbl, file = glue('../rds/tbl_gid/tbl_{x}.rds'))
  ids <- 1:nrow(tbl)
  # To calculate the PET
  myFunction <- function(i){
    d <- tbl %>% filter(id == i)
    har <- hargreaves(Tmin = pull(d, tmin), Tmax = pull(d, tmax), Pre = pull(d, prec), lat = unique(d$y))
    d <- d %>% mutate(etp = har)  
    print(glue('Done {i}'))
    return(d)
  }
  pet <- lapply(1:length(ids), function(k) myFunction(i = ids[k]))
  pet <- bind_rows(pet)
  saveRDS(object = pet, file = glue('../rds/etp/pet_{x}.rds'))
  return(pet)
}
