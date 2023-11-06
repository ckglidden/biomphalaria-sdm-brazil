library(sf)
library(raster)
library(dplyr)
library(stringr)
library(fasterize)

setwd("/Users/cglidden/Documents/schisto_project/schisto-brazil-sdm-repo/raw-data/gee-data")

# read data
grid <- st_read('1km_grid_brazil/1km_grid_brazil.shp')

lulc_file <- read.csv("../env-rasters/lulc-tiffs/brazil_lulc_1992.csv")

##-------------------------------------------##
## clean ag mosiac & temp crops              ##
##-------------------------------------------##
# lulc_file$class[lulc_file$class == 0] <- "not_defined"
# lulc_file$class[lulc_file$class == "21"] <- "ag_mosiac"
# lulc_file$class[lulc_file$class == "41"] <- "temp_crops"

rep_str <- c("0" = "not_defined", "21" = "ag_mosiac", "41" = "temp_crops")

lulc_file <- lulc_file %>% 
  mutate(class = str_replace_all(class, rep_str))
         
##then need to transpose long to wide.
lulc_crops_wide <- lulc_file[c(2:4)] %>% #group_by(col_code) %>%
  #mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = class, values_from = area) 

lulc_crops_wide[is.na(lulc_crops_wide)] <- 0

##----------------------------##
## merge to grid              ##
##-----------------------------##
lulc_grid <- left_join(grid, lulc_crops_wide, by = "row_code")
st_write(lulc_grid, '1km_ag_lulc_brazil.shp')


##----------------------------##
## make rasters.               ##
##-----------------------------##

raster <- raster('../env-rasters/base-data/schisto_base_variables.tif')

ag_mosiac <- fasterize(lulc_grid, raster, 'ag_mosiac')
writeRaster(ag_mosiac, '../env-rasters/lulc-tiffs/ag_mosiac_1992.tif')

temp_crops <- fasterize(lulc_grid, raster, 'temp_crops')
writeRaster(temp_crops, '../env-rasters/lulc-tiffs/temp_crops_1992.tif')



