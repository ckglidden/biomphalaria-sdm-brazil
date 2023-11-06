### this code creates prediction maps for schisto paper

setwd("/Users/carolineglidden/Documents/shisto-project")
library(raster)
library(RColorBrewer)
library(viridis)
library(paletteer)

##----------------------------##
## set up colors.             ##
##----------------------------##

#palettes
mean_pal <- viridis(7, option = "D", direction = -1)
sd_pal <- viridis(7, option = "A", direction = -1)

# pal <- c("grey93", "grey93", pal0[2:7])
difference_pal0<-  paletteer_c("ggthemes::Red-Blue Diverging", 20)
difference_pal <- c(difference_pal0[1:8], "grey93", "grey93", difference_pal0[13:20])

#pal3 <- viridis::viridis(10, option = "C") 

##---------------------------------##
## historical distribution (1992)  ##
##---------------------------------##
hist_files <- list.files("straminea/straminea/geoTiffs", pattern="hist_raster_.*\\.tif$", full.names=TRUE) # get list of file names

hist_rasters <- stack(hist_files) # read in & stack

# get summary rasters
hist_mean <- calc(hist_rasters, fun = mean)
hist_sd <- calc(hist_rasters, fun = sd)

# save if want for later
writeRaster(hist_mean, "straminea/summarized-geoTiffs/straminea_predicted_mean_distribution_1992.tif")
writeRaster(hist_sd, "straminea/summarized-geoTiffs/straminea_standard_dev_predicted_distribution_1992.tif")

# create figure
plot(hist_mean, # update this to min max transformation
     zlim = c(0, 1), 
     col = mean_pal)
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

##----------------------------##
## current distribution.      ##
##----------------------------##
current_files <- list.files("straminea/straminea/geoTiffs", pattern="current_raster_.*\\.tif$", full.names=TRUE) # get list of file names

current_rasters <- stack(current_files) # read in & stack

# get summary rasters
current_mean <- calc(current_rasters, fun = mean, na.rm = TRUE)
current_sd <- calc(current_rasters, fun = sd, na.rm = TRUE)

# save if want for later
writeRaster(current_mean, "straminea/summarized-geoTiffs/straminea_predicted_mean_distribution_2017.tif")
writeRaster(current_sd, "straminea/summarized-geoTiffs/straminea_standard_dev_predicted_distribution_2017.tif")

# create figure
plot(current_mean, # update this to min max transformation
     zlim = c(0, 1), 
     col = mean_pal)
scalebar(500,
         below = 'km',
         xy = c(-45, -30))


##----------------------------##
## climate counterfactual     ##
##----------------------------##
climate_files <- list.files("straminea/straminea/geoTiffs", pattern="climate_counterfactual_.*\\.tif$", full.names=TRUE) # get list of file names

climate_rasters <- stack(climate_files) # read in & stack

# get summary rasters
climate_mean <- calc(climate_rasters, fun = mean, na.rm = TRUE)
climate_sd <- calc(climate_rasters, fun = sd, na.rm = TRUE)

# save if want for later
writeRaster(climate_mean, "straminea/summarized-geoTiffs/straminea_predicted_mean_distribution_climateCF.tif")
writeRaster(climate_sd, "straminea/summarized-geoTiffs/straminea_standard_dev_predicted_distribution_climateCF.tif")


##--------------------------##
## urban counterfactual     ##
##--------------------------##
urban_files <- list.files("straminea/straminea/geoTiffs", pattern="urban_counterfactual_.*\\.tif$", full.names=TRUE) # get list of file names

urban_rasters <- stack(urban_files) # read in & stack

# get summary rasters
urban_mean <- calc(urban_rasters, fun = mean, na.rm = TRUE)
urban_sd <- calc(urban_rasters, fun = sd, na.rm = TRUE)

# save if want for later
writeRaster(urban_mean, "straminea/summarized-geoTiffs/straminea_predicted_mean_distribution_urbanCF.tif")
writeRaster(urban_sd, "straminea/summarized-geoTiffs/straminea_standard_dev_predicted_distribution_urbanCF.tif")


##------------------------------##
## difference national figures  ##
##------------------------------##

## observed difference
plot(current_mean - hist_mean, 
     col = mean_pal)
scalebar(500,
         below = 'km',
         xy = c(-45, -30))


## climate counter-factual
plot(current_mean - climate_mean, 
     col = difference_pal)
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

## urban counter-factual
plot(current_mean - urban_mean, 
     col = difference_pal)
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

##---------------------##
## standard deviation  ##
##---------------------##
plot(hist_sd, 
     col = sd_pal,
     main = substitute(paste('a. ', italic('B. straminea'), " 1992")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

plot(current_sd, 
     col = sd_pal,
     main = substitute(paste('b. ', italic('B. straminea'), " 2017")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

plot(climate_sd, 
     col = sd_pal,
     main = substitute(paste('c. ', italic('B. straminea'), " climate counterfactual")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

plot(urban_sd, 
     col = sd_pal,
     main = substitute(paste('d. ', italic('B. straminea'), " urban counterfactual")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

##----------------------##
## regional figures     ##
##----------------------##

# IDENTIFY CITIES AND WHERE POPULATION SIZE / CITY DISTRIBUTION HAS CHANGED
