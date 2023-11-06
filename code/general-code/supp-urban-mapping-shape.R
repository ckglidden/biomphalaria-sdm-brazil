##### code to get shapefiles for urban microregions

library(geobr)
library(sf)

# microregions

# microregions <- read_micro_region(code_micro = "all", year = "2018") 

# minas gerais microregion

belo_micro <- read_micro_region(code_micro = 31030, year = "2018")
  
# sao paulo microregion
  
sao_micro <- read_micro_region(code_micro = 35061, year = "2018")
  
# change urban shapes
urban_examples <- rbind(belo_micro, sao_micro)

#
st_write(urban_examples, "urban_examples.shp")

test <- raster::raster('/Users/carolineglidden/Documents/shisto-project/general-code/regional-summarized-tiffs/glabrata_urban_change_belo.tif')
plot(test)
