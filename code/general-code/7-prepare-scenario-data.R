# Code to prepare rasters to create historical predictions, current predictions, and counterfactual scenarios

##------------------------------------##
## set wd & load necessary packages.  ##
##------------------------------------##

library(raster)
setwd("biomphalaria-sdm-brazil") # set path based on where you save it on local machine

##----------------------------------------------------##
## create clunky function to transform climate vars   ##
##----------------------------------------------------##
transform_bios <- function(data) {
  #temp
  data$bio2 <- data$bio2 * 0.1
  data$bio3 <- data$bio3 * 0.1
  data$bio8 <- (data$bio8 * 0.1) - 273.15
  data$bio9 <- (data$bio9 * 0.1) - 273.15
  
  #precip
  data$bio12 <- data$bio12 * 0.1
  data$bio13 <- data$bio13 * 0.1
  data$bio14 <- data$bio14 * 0.1
  data$bio15 <- data$bio15 * 0.1
  data$bio16 <- data$bio16 * 0.1
  
  return(data)
}
  
  
##------------------##
## load base data   ##
##------------------##

base_data <- stack('raw-data/env-rasters/base-data/schisto_base_variables.tif')


##----------------------------##
## load & prepare 1992 data   ##
##----------------------------##

# load rasters
hist_data <- stack('raw-data/env-rasters/historical-data/observed_1992_climate_urban.tif')

hist_env_data <- stack(c(base_data, hist_data))

# convert to df & remove rows with missing values
historical_prediction_df <- as.data.frame(rasterToPoints(hist_env_data)) ##this gets raster value for every grid cell of Brazil
hist_prediction_df_complete <- historical_prediction_df[complete.cases(historical_prediction_df), ] #; prediction_df_complete <- prediction_df_complete[,-1]

# rescale & calc climate variables
hist_prediction_df_complete <- transform_bios(hist_prediction_df_complete)

# transform urban gradient & remove rural
# add a tiny value so that we don't get inf (0.1)
hist_prediction_df_complete <- hist_prediction_df_complete %>% mutate(dist_int_urbanAR =  if_else(ar_urban_gradient == 0, 0.1,  ar_urban_gradient),
                                                                      dist_high_urban = if_else(high_urban_gradient == 0, 0.1,  high_urban_gradient),
                                                                      dist_rural = if_else(rural_1992 == 0, 0.1,  rural_1992))
# summary(hist_prediction_df_complete) # check to make sure this worked before moving on

# standardize 0-1 (0 = rural, 1 = urban)
hist_prediction_df_complete <- hist_prediction_df_complete %>% mutate(ar_urban_gradient =  (((dist_int_urbanAR/dist_rural) - 0.000002)/(500000 - 0.000002)),
                                high_urban_gradient = (((dist_high_urban/dist_rural) - 0.000002)/(500000 - 0.000002)))
summary(hist_prediction_df_complete) # check to make sure this worked before moving on

# remove variables that won't be used
hist_prediction_df_complete <- hist_prediction_df_complete %>% dplyr::select(-c('dist_int_urbanAR', 'dist_high_urban', 'dist_rural'))

# save rds for later analysis
saveRDS(hist_prediction_df_complete, "raw-data/env-rasters/prediction-map-rds/sdm_data_1992.rds")

##----------------------------##
## load & prepare 2017 data   ##
##----------------------------##

# load rasters
current_data <- stack('raw-data/env-rasters/current-data/observed_2017_climate_urban.tif')

current_env_data <- stack(c(base_data, current_data))

# convert to df & remove rows with missing values
current_prediction_df <- as.data.frame(rasterToPoints(current_env_data)) ##this gets raster value for every grid cell of Brazil
current_prediction_df_complete <- current_prediction_df[complete.cases(current_prediction_df), ] #; prediction_df_complete <- prediction_df_complete[,-1]

# rescale & calc climate variables
current_prediction_df_complete <- transform_bios(current_prediction_df_complete)

# transform urban gradient & remove rural
# add a tiny value so that we don't get inf (0.1)
current_prediction_df_complete <- current_prediction_df_complete %>% mutate(dist_int_urbanAR =  if_else(ar_urban_gradient == 0, 0.1,  ar_urban_gradient),
                                                                      dist_high_urban = if_else(high_urban_gradient == 0, 0.1,  high_urban_gradient),
                                                                      dist_rural = if_else(rural_2017 == 0, 0.1,  rural_2017))
# summary(current_prediction_df_complete) # check to make sure this worked before moving on

# standardize 0-1 (0 = rural, 1 = urban)
current_prediction_df_complete <- current_prediction_df_complete %>% mutate(ar_urban_gradient =  (((dist_int_urbanAR/dist_rural) - 0.000002)/(500000 - 0.000002)),
                                                                      high_urban_gradient = (((dist_high_urban/dist_rural) - 0.000002)/(500000 - 0.000002)))
summary(current_prediction_df_complete) # check to make sure this worked before moving on

# remove variables that won't be used
current_prediction_df_complete <- current_prediction_df_complete %>% dplyr::select(-c('dist_int_urbanAR', 'dist_high_urban', 'dist_rural'))

# save rds for later analysis
saveRDS(current_prediction_df_complete, "raw-data/env-rasters/prediction-map-rds/sdm_data_2017.rds")

##----------------------------------------------##
## load & prepare climate counterfactual data   ##
##----------------------------------------------##

# load rasters
climate_data <- stack('raw-data/env-rasters/counterfactuals/climate_counterfactual_vars.tif')

climate_env_data <- stack(c(base_data, climate_data))

# convert to df & remove rows with missing values
climate_prediction_df <- as.data.frame(rasterToPoints(climate_env_data)) ##this gets raster value for every grid cell of Brazil
climate_prediction_df_complete <- climate_prediction_df[complete.cases(climate_prediction_df), ] #; prediction_df_complete <- prediction_df_complete[,-1]

# rescale & calc climate variables
climate_prediction_df_complete <- transform_bios(climate_prediction_df_complete)

# transform urban gradient & remove rural
# add a tiny value so that we don't get inf (0.1)
climate_prediction_df_complete <- climate_prediction_df_complete %>% mutate(dist_int_urbanAR =  if_else(ar_urban_gradient == 0, 0.1,  ar_urban_gradient),
                                                                            dist_high_urban = if_else(high_urban_gradient == 0, 0.1,  high_urban_gradient),
                                                                            dist_rural = if_else(rural_2017 == 0, 0.1,  rural_2017))
# summary(climate_prediction_df_complete) # check to make sure this worked before moving on

# standardize 0-1 (0 = rural, 1 = urban)
climate_prediction_df_complete <- climate_prediction_df_complete %>% mutate(ar_urban_gradient =  (((dist_int_urbanAR/dist_rural) - 0.000002)/(500000 - 0.000002)),
                                                                            high_urban_gradient = (((dist_high_urban/dist_rural) - 0.000002)/(500000 - 0.000002)))
summary(climate_prediction_df_complete) # check to make sure this worked before moving on

# remove variables that won't be used
climate_prediction_df_complete <- climate_prediction_df_complete %>% dplyr::select(-c('dist_int_urbanAR', 'dist_high_urban', 'dist_rural'))

# save rds for later analysis
saveRDS(climate_prediction_df_complete, "raw-data/env-rasters/prediction-map-rds/sdm_climate_counterfactual.rds")

##----------------------------------------------##
## load & prepare urban counterfactual data   ##
##----------------------------------------------##

# load rasters
urban_data <- stack('raw-data/env-rasters/counterfactuals/urban_counterfactual_vars.tif')

urban_env_data <- stack(c(base_data, urban_data))

# convert to df & remove rows with missing values
urban_prediction_df <- as.data.frame(rasterToPoints(urban_env_data)) ##this gets raster value for every grid cell of Brazil
urban_prediction_df_complete <- urban_prediction_df[complete.cases(urban_prediction_df), ] #; prediction_df_complete <- prediction_df_complete[,-1]

# rescale & calc climate variables
urban_prediction_df_complete <- transform_bios(urban_prediction_df_complete)

# transform urban gradient & remove rural
# add a tiny value so that we don't get inf (0.1)
urban_prediction_df_complete <- urban_prediction_df_complete %>% mutate(dist_int_urbanAR =  if_else(ar_urban_gradient == 0, 0.1,  ar_urban_gradient),
                                                                            dist_high_urban = if_else(high_urban_gradient == 0, 0.1,  high_urban_gradient),
                                                                            dist_rural = if_else(rural_1992 == 0, 0.1,  rural_1992))
# summary(urban_prediction_df_complete) # check to make sure this worked before moving on

# standardize 0-1 (0 = rural, 1 = urban)
urban_prediction_df_complete <- urban_prediction_df_complete %>% mutate(ar_urban_gradient =  (((dist_int_urbanAR/dist_rural) - 0.000002)/(500000 - 0.000002)),
                                                                            high_urban_gradient = (((dist_high_urban/dist_rural) - 0.000002)/(500000 - 0.000002)))
summary(urban_prediction_df_complete) # check to make sure this worked before moving on

# remove variables that won't be used
urban_prediction_df_complete <- urban_prediction_df_complete %>% dplyr::select(-c('dist_int_urbanAR', 'dist_high_urban', 'dist_rural'))

# save rds for later analysis
saveRDS(urban_prediction_df_complete, "raw-data/env-rasters/prediction-map-rds/sdm_urban_counterfactual.rds")

##----------------------------------------------------------##
## double check units of bio correct across all datasets.  ##
##----------------------------------------------------------##
summary(hist_prediction_df_complete)
summary(current_prediction_df_complete)
summary(climate_prediction_df_complete)
summary(urban_prediction_df_complete)


