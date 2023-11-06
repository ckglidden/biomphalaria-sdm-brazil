## code to clean test data for B. glabrata model (1992-2000 data)

##-------------------------------------------##
## set wd & load libraries                   ##
##-------------------------------------------##
setwd("/Users/cglidden/Documents/schisto_project/schisto-brazil-sdm-repo")
library(tidyverse); library(dplyr); library(sf); library(raster)
library(enmSdmX); library(spatialsample);library(stringr); 
library(dismo); library(foreach); library(caret)

##-------------------------------------------##
## load data & restructure to thin           ##
##-------------------------------------------##
environmental_covariate_df <- readRDS("clean-data/test_data_sdm_oct18.rds")
environmental_covariate_df <- environmental_covariate_df[, c("latitude", "longitude", names(environmental_covariate_df)[1:48])]

#subset to 1990-1999 for training and validation
environmental_covariate_df <- subset(environmental_covariate_df, year < 2000)

##---------------------------------------##
## remove highly correlated variables    ##
##---------------------------------------##

##examine correlation from full dataset -- remove orginal variables before chaning distance to urban (these are uncorrelated, cor = 0.58)
# corr_m <- cor(environmental_covariate_df[c(3, 6:9, 11:13, 15,16, 19:22, 24:44, 45:49)])
# cor_cols <- findCorrelation(corr_m, cutoff = .75, exact = TRUE, verbose = TRUE)
# names(environmental_covariate_df[c(3, 6:9, 11:13, 15,16, 19:22, 24:44, 45:49)])[cor_cols]
to_remove <- c( "bio1", "bio4", "bio5", "bio6", "bio7", "bio10", "bio11", "bio17", "bio18", "bio19", "waterOccur", "waterSeaso",  "gHM", "slope", "bio4a")

environmental_covariate_df <- environmental_covariate_df[,!(names(environmental_covariate_df) %in% to_remove)]
hist_environmental_covariate_df <- environmental_covariate_df %>% na.omit()

##-----------------------------------------------------------------##
## get 1 obs per grid cell for glabarta occurrence & background    ##
##-----------------------------------------------------------------##
hist_environmental_covariate_df$presence <- ifelse(grepl("glabrata", hist_environmental_covariate_df$species), "1", "0")

##thin so that only one point per pixel, retaining presence over absence
rast <- raster("raw-data/env-rasters/base-data/elevation.tif")

#one presence point per grid cell
presence_hist <- subset(hist_environmental_covariate_df, presence > 0)
s <- gridSample(presence_hist[c(2,1)], rast, n=1)
thin.occ <- presence_hist[row.names(s),] #40 obs

occ_hist_sf <- st_as_sf(thin.occ, coords = c("longitude","latitude"),
                        agr = "constant", crs = 4326)

#one background point per grid cell
bkg_hist <- subset(hist_environmental_covariate_df, presence < 1)
ss<- gridSample(bkg_hist[c(2,1)], rast, n=1)
thin.bkg <- bkg_hist[row.names(ss),] #1688 obs

bkg_hist_sf <- st_as_sf(thin.bkg, coords = c("longitude","latitude"),
                        agr = "constant", crs = 4326)

#### combine points so that bkg points do not include presence points
#1km buffer around presence points
df_buff <- st_buffer(occ_hist_sf, dist = 1000)

#ask if points intersect buffer
mat <- st_intersects(bkg_hist_sf, df_buff, sparse = FALSE); bkg_hist_sf$int <- apply(mat, 1, any)
bkg_hist_sf_final <- subset(bkg_hist_sf, int == FALSE); bkg_hist_sf_final <- st_drop_geometry(bkg_hist_sf_final)


##---------------------------------------------------------##
## reduce dataset to columns needed for analysis & save    ##
##---------------------------------------------------------##

drop_vars <- c("total_population", "total_nightlights", "not_defined", 
               "row_code", "year", "source", "origRC", "species", "dataset", 
                "longitude", "latitude", "int")

thin.occ.final <- thin.occ[,!(names(thin.occ) %in% drop_vars)]
bkg_hist_sf_final <- bkg_hist_sf_final[,!(names(bkg_hist_sf_final) %in% drop_vars)]

# combine background and presence
analysis_data_final <- rbind(thin.occ.final, bkg_hist_sf_final)

# split dataset
analysis_data_final_AR <- analysis_data_final[,!(names(analysis_data_final) %in% "un_urban_gradient")]
analysis_data_final_UN <- analysis_data_final[,!(names(analysis_data_final) %in% "ar_urban_gradient")]

saveRDS(analysis_data_final_AR, "clean-data/glabrata/glabrata_AR_test_data_oct18.rds")
saveRDS(analysis_data_final_UN, "clean-data/glabrata/glabrata_UN_test_data_oct18.rds")







 







































##old feature selection code

# #------------------------------------------------#
# # update temp vars based on thermal response     #
# #------------------------------------------------#
# # from : 'code/temperature_response_functions.R'
# #FINAL RECRUITMENT FUNCTION
# recruitment_function <- function (temp) { #x = temp
#   est <- 200.2 * exp(-0.5 * (abs(temp - 22.12)/3.609)^2)
#   return(est)
# }
# 
# #bio1, bio5, bio6 
# 
# analysis_data$mean_td_recruitment <- foreach(i = 1:nrow(analysis_data), .combine = c) %do%
#   (recruitment_function(analysis_data[i, "bio1"]))
#   
# analysis_data$max_td_recruitment <- foreach(i = 1:nrow(analysis_data), .combine = c) %do%
#   (recruitment_function(analysis_data[i, "bio5"]))
# 
# analysis_data$min_td_recruitment <- foreach(i = 1:nrow(analysis_data), .combine = c) %do%
#   (recruitment_function(analysis_data[i, "bio6"]))
# 
# analysis_data$recruitment_range <- analysis_data$max_td_recruitment - analysis_data$min_td_recruitment
# 
# climate_cor <- cor(analysis_data[,c("mean_td_recruitment", "min_td_recruitment", "max_td_recruitment", "recruitment_range")])
# 
# corrplot::corrplot(climate_cor, method="number")
# 
# #exclude min b/c correlated w/mean
# 
# #------------------------------------------------#
# # select final precip variables based on pca     #
# #------------------------------------------------#
# 
# # climate.pca <- prcomp(analysis_data[,c(27:34)], center = TRUE,scale. = TRUE)
# # summary(climate.pca) #5 PCA to capture variation > 93% of variation
# # pca_data <- as.data.frame(climate.pca$rotation); pca_data$variables <- as.vector(rownames(pca_data)); pca_data <- pca_data[, c(1:5, 9)]
# # 
# # View(pca_data) #pc1 = bio6, pc2 = bio19, pc3 = bio13, pc4 = bio5, pc5 = bio2
# 
# #just include bio12 bc it is the most interp,
# 
# climate_vars <- c("bio12", "mean_td_recruitment", "max_td_recruitment", "recruitment_range")
# 
# analysis_data_climate <- analysis_data[, (names(analysis_data) %in% climate_vars)]
# 
# m0 <- cor(analysis_data_climate)
# 
# corrplot::corrplot(m0, method="number")
# 
# #-------------------------------------------------------------------------#
# # Now remove highly correlated variables for other types of traits        #
# #-------------------------------------------------------------------------#
# 
# m <- cor(analysis_data[, c(6:9, 11:16, 19:22, 24, 45:52)])
# 
# corrplot::corrplot(m, method="circle")
# 
# drop_vars <- c("elevation", "gHM", "waterSeaso", "waterRecur", "total_population", "total_nightlights",
#                "not_defined", "row_code", "year", "source", "origRC", "species", "dataset", 
#                "longitude", "latitude")
# 
# analysis_data_final <- analysis_data[, !(names(analysis_data) %in% drop_vars)]
# 
# analysis_data_final <- cbind(analysis_data_final[,c(1:12, 33:39)], analysis_data_climate)










