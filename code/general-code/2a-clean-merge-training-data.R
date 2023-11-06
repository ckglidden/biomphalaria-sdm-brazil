#### This code cleans the environmental data downloaded from GEE for a training and validationa dataset (2017-2020)

##-------------------------------------------##
## set wd & load libraries                   ##
##-------------------------------------------##
setwd("/Users/cglidden/Documents/schisto_project/schisto-brazil-sdm-repo")

library(dplyr)
library(tidyr)
library(sf)

##-------------------------------------------##
## read in data                              ##
##-------------------------------------------##

#####read in data for int hosts
misc_vars <- st_read("raw-data/gee-data/all_points_sdm_misc_vars")
lulc_crops <- read.csv("raw-data/gee-data/all_points_sdm_lulc_crops.csv")
climate <- read.csv("raw-data/gee-data/all_sdm_points_chelsa_decadal.csv")
un_urban_dist <- read.csv('raw-data/gee-data/cleaned_schisto_urbanCC_UN_oct102023.csv')
ar_urban_dist <- read.csv('raw-data/gee-data/cleaned_schisto_urbanCC_AR_oct102023.csv')
rural_distance <- read.csv('raw-data/gee-data/cleaned_schisto_ruralCC_oct102023.csv')

########## 'training_urban_dist' goes from 2000-2017 so the final dataframe goes from 2000-2017

##-------------------------------------------##
## clean ag mosiac & temp crops              ##
##-------------------------------------------##

lulc_crops$class[lulc_crops$class == 0] <- "not_defined"
lulc_crops$class[lulc_crops$class == 21] <- "ag_mosiac"
lulc_crops$class[lulc_crops$class == 41] <- "temp_crops"

##subset by relevant years beforehand
ids <- misc_vars[,c("year","row_code")]; ids <- unique(ids)
lulc_crops_v2 <- left_join(lulc_crops, ids, by = "row_code")
lulc_crops_v3 <- lulc_crops_v2 %>% filter(year.x == year.y); lulc_crops_v4 <- lulc_crops_v3[2:4]

##then need to transpose long to wide.
lulc_crops_wide <- lulc_crops_v4 %>% #group_by(col_code) %>%
  #mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = class, values_from = area) 

lulc_crops_wide[is.na(lulc_crops_wide)] <- 0

##-------------------------------------------##
## clean climate data                        ##
##-------------------------------------------##

climate_long_v1 <- climate[,c(2:82)] %>% 
  tidyr::pivot_longer(cols = starts_with("bio"), 
                      names_to = "var",
                      values_to = "value")

climate_long_v1 <- climate_long_v1 %>%
  tidyr::separate(var, c("bioClim", "year.bio", "extra"), "_") 

climate_long_v1 <- climate_long_v1[,-5]

# assign climate decade
climate_long_v1$time_frame <- ifelse(climate_long_v1$year < 1991, 1981,
                                     ifelse(climate_long_v1$year > 1990 & climate_long_v1$year < 2001, 1991,
                                     ifelse(climate_long_v1$year > 2000 & climate_long_v1$year < 2011, 2001,
                                            ifelse(climate_long_v1$year > 2010, 2011, NA)))) # update to include until 2020?

climate_filtered <- climate_long_v1 %>% filter(time_frame == year.bio)

##then need to transpose long to wide.
climate_filtered_wide <- climate_filtered %>% #group_by(col_code) %>%
  #mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = bioClim, values_from = value) 

#resale properly using CHELSA key - check with histogram
climate_filtered_wide$bio1 <- (climate_filtered_wide$bio1*0.1) - 273.15
climate_filtered_wide$bio2 <- climate_filtered_wide$bio2 * 0.1
climate_filtered_wide$bio3 <- climate_filtered_wide$bio3 * 0.1
climate_filtered_wide$bio4 <- climate_filtered_wide$bio4 * 0.1
climate_filtered_wide$bio5 <- (climate_filtered_wide$bio5*0.1) - 273.15
climate_filtered_wide$bio6 <- (climate_filtered_wide$bio6*0.1) - 273.15
climate_filtered_wide$bio7 <- climate_filtered_wide$bio7 * 0.1
climate_filtered_wide$bio8 <- (climate_filtered_wide$bio8 * 0.1) - 273.15
climate_filtered_wide$bio9 <- (climate_filtered_wide$bio9 * 0.1) - 273.15
climate_filtered_wide$bio10 <- (climate_filtered_wide$bio10 * 0.1) - 273.15
climate_filtered_wide$bio11 <- (climate_filtered_wide$bio11 * 0.1) - 273.15
climate_filtered_wide$bio12 <- climate_filtered_wide$bio12 * 0.1
climate_filtered_wide$bio13 <- climate_filtered_wide$bio13 * 0.1
climate_filtered_wide$bio14 <- climate_filtered_wide$bio14 * 0.1
climate_filtered_wide$bio15 <- climate_filtered_wide$bio15 * 0.1
climate_filtered_wide$bio16 <- climate_filtered_wide$bio16 * 0.1
climate_filtered_wide$bio17 <- climate_filtered_wide$bio17 * 0.1
climate_filtered_wide$bio18 <- climate_filtered_wide$bio18 * 0.1
climate_filtered_wide$bio19 <- climate_filtered_wide$bio19 * 0.1

##-------------------------------------------------------------------##
## combine urban datasets & standardize by rural                     ##
##-------------------------------------------------------------------##

# remove high density from one data set so it is not repeated & join
distance <- left_join(un_urban_dist, ar_urban_dist[,-3], by = c("year", "row_code")) %>% 
  left_join(rural_distance, by = c("year", "row_code"))

# add a tiny value so that we don't get inf (0.1)
distance <- distance %>% mutate(dist_int_urban = if_else(dist_int_urban == 0, 0.1, dist_int_urban),
                                dist_int_urbanAR =  if_else(dist_int_urbanAR == 0, 0.1,  dist_int_urbanAR),
                                dist_high_urban = if_else(dist_high_urban == 0, 0.1,  dist_high_urban),
                                dist_rural = if_else(dist_rural == 0, 0.1,  dist_rural))
summary(distance) # check to make sure this worked before moving on

# standardize 0-1 (0 = rural, 1 = urban) ###### NEED TO ADD IN HIGH URBAN
distance <- distance %>% mutate(un_urban_gradient = (((dist_int_urban/dist_rural) - 0.000002)/(500000 - 0.000002)),
                                ar_urban_gradient =  (((dist_int_urbanAR/dist_rural) - 0.000002)/(500000 - 0.000002)),
                                high_urban_gradient = (((dist_high_urban/dist_rural) - 0.000002)/(500000 - 0.000002)))
summary(distance) # check to make sure this worked before moving on

# check correlation among all variables
names(distance)
cor_mat <- cor(distance[3:ncol(distance)])
corrplot::corrplot(cor_mat, method = "number")

hist(distance$un_urban_gradient)
hist(distance$ar_urban_gradient)
hist(distance$high_urban_gradient)


##-------------------------------------------##
## combine all datasets.                     ##
##-------------------------------------------##

final_data_rds <- left_join(misc_vars, climate_filtered_wide[,c(1, 5:ncol(climate_filtered_wide))], by = "row_code")
final_data_rds <- left_join(final_data_rds, lulc_crops_wide, by = "row_code")
final_data_rds <- left_join(final_data_rds, distance[,c('row_code', 'un_urban_gradient', 'ar_urban_gradient', 'high_urban_gradient')], by = "row_code")

summary(final_data_rds) # make sure nothing looks weird

# final_data_rds_complete <- final_data_rds[!sf::st_is_empty(final_data_rds), ] %>% na.omit()

#add lat & long
centroids <- st_transform(final_data_rds, 4326) %>% 
  st_centroid()

separated_coord <- centroids %>%
  dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                latitude = sf::st_coordinates(.)[,2])

final_data_lat_long <- st_drop_geometry(separated_coord)

##take out weird climate values
final_data_lat_long <- subset(final_data_lat_long, bio5 > 0) # this is only a couple of values
summary(final_data_lat_long)

#save data
saveRDS(final_data_lat_long, "clean-data/training_data_sdm_oct12.rds")


