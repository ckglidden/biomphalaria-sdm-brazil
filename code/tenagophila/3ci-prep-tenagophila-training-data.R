#### PREP TENAGOPHILA DATA

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
environmental_covariate_df <- readRDS("clean-data/training_data_sdm_oct12.rds")
environmental_covariate_df <- environmental_covariate_df[, c("latitude", "longitude", names(environmental_covariate_df)[1:48])]

#subset to 2000-2020 for training and validation
environmental_covariate_df <- subset(environmental_covariate_df, year > 1999 & year < 2021)

##---------------------------------------##
## remove highly correlated variables    ##
##---------------------------------------##

##examine correlation from full dataset -- remove orginal variables before chaning distance to urban (these are uncorrelated, cor = 0.58)
# corr_m <- cor(environmental_covariate_df[c(3, 6:9, 11:13, 15,16, 19:22, 24:44, 45:49)])
# cor_cols <- findCorrelation(corr_m, cutoff = .75, exact = TRUE, verbose = TRUE)
# names(environmental_covariate_df[c(3, 6:9, 11:13, 15,16, 19:22, 24:44, 45:49)])[cor_cols]
to_remove <- c( "bio1", "bio4", "bio5", "bio6", "bio7", "bio10", "bio11", "bio17", "bio18", "bio19", "waterOccur", "waterSeaso",  "gHM", "slope", "bio4a")

environmental_covariate_df <- environmental_covariate_df[,!(names(environmental_covariate_df) %in% to_remove)]
environmental_covariate_df <- environmental_covariate_df %>% na.omit()

##--------------------------------##
## Thin target species df         ##
##--------------------------------##

# set target species
# target_species_string <- 'tenagophila' #, 'glabrata', 'straminea'

occ.target <- environmental_covariate_df %>% 
  filter(str_detect(species, "tenagophila")) #%>% #filter by target species

occ_target_lat_lon <- as.data.frame(occ.target[,c("species", "longitude", "latitude")]); occ_target_lat_lon$latitude <- as.numeric(occ_target_lat_lon$latitude); occ_target_lat_lon$longitude <- as.numeric(occ_target_lat_lon$longitude)

rast <- raster("raw-data/env-rasters/base-data/elevation.tif")

#one point per grid cell
set.seed(9)
s <- gridSample(occ_target_lat_lon[2:3], rast, n=1)
thin.occ <- occ.target[row.names(s),]; thin.occ <- thin.occ[complete.cases(thin.occ), ] #173 total
thin.occ$presence <- 1


##-----------------------------------------------------##
## Build background species / background mask rows     ##
##-----------------------------------------------------##

# Limit to background species (non target species, non wo specification)
bg_df <- environmental_covariate_df %>%
  filter(str_detect(species, "tenagophila", negate = TRUE))

# Read in template raster and list setup
bg_species_list <- unique(bg_df$species)

# Extract number of background points per grid cell (i.e., weighted bias mask)
bg_points <- bg_df %>% dplyr::select(c(longitude, latitude)) %>%
  as.matrix()

bg_df$index <- c(1:dim(bg_df)[1])

bg_longlat <- cellFromXY(rast, bg_points) %>% as.data.frame() %>%
  cbind(bg_df$year) %>%
  cbind(bg_df$index) %>%
  mutate(count = 1) %>% setNames(c("cell","year","index","count")) %>%
  group_by(cell) %>% dplyr::summarize(count = sum(count),
                                      max_year = max(year),
                                      # avg_year = mean(year),
                                      max_index = max(index)) %>%
  # arrange(desc(count)) %>%
  mutate(longitude = xFromCell(rast, cell),  # Acquire longitude (x) and latitude (y) from cell centroids
         latitude = yFromCell(rast, cell)) %>%
  dplyr::select(-cell) %>% # Cell number is now obsolete, since will be working from (x,y) as an sf object
  filter(!is.na(longitude) & !is.na(latitude)) # Remove the NA locations

# Build geometries
bg_mask_sf_full <- st_as_sf(bg_longlat, coords = c("longitude","latitude"),
                            agr = "constant", crs = 4326) 

# Random sample bg without replacement from weighted bias mask at (2x occ) multiplier
set.seed(9)
multiplier <- 2

bg_mask_weights <- bg_mask_sf_full %>%
  mutate(weight = count/sum(count))

bg_mask_df <- bg_mask_sf_full[sample(nrow(bg_mask_weights),
                                     size = multiplier * nrow(thin.occ),
                                     replace = FALSE,
                                     prob = bg_mask_weights$weight),]

bg_final_data <- bg_df[bg_mask_df$max_index,]
bg_final_data$presence <- 0

#combine data
names(bg_final_data)
all_data <- rbind(thin.occ, bg_final_data[-36]) #"index" #519 total
all_data$new_row_code <- seq(1, nrow(all_data), by = 1)

##----------------------------------------##
## create folds for 5-fold spatial CV     ##
##----------------------------------------##

all_data_sf <- st_as_sf(all_data, coords = c("longitude","latitude"),
                        agr = "constant", crs = 4326) 

#identify  groups  of  5  clusters  using  the  spatialsample  package
set.seed(99)  #set  seed  to  get  same  split  each  time
clusters  <-  spatial_block_cv(all_data_sf, 
                               method = "snake", n = 25, #method for how blocks are oriented in space & number of blocks
                               relevant_only = TRUE,  v = 5)  #k-means  clustering  to  identify  cross-validation  folds  (3  is  too  few  to  be  robust  but  using  here  to  save  time)

#for  loop  to  create  a  dataframe  that  assigns  a  fold  number  to  each  data  point
splits_df  <-  c()
for(i  in  1:5){
  new_df  <-  assessment(clusters$splits[[i]])  #extract  points  in  fold  number  i
  new_df$fold  <-  i
  new_df  <-  new_df[, c("new_row_code", "fold")]
  splits_df  <-  rbind(splits_df, new_df)  #bind  all  points  x  fold  id  together
}

splits_df  <-  st_drop_geometry(splits_df)  #drop geo to make dataframe

#final  data  -  merge  cluster  id  to  final  dataset  for  analysis
analysis_data  <-  merge(all_data, splits_df, by  =  "new_row_code")
analysis_data <- analysis_data[, -1]

#sanity  check:  check  how  many  data  points  are  in  each  fold for each response
table(analysis_data$fold, analysis_data$presence)

##---------------------------------------------------------##
## reduce dataset to columns needed for analysis & save    ##
##---------------------------------------------------------##

drop_vars <- c("total_population", "total_nightlights", "not_defined", 
               "row_code", "year", "source", "origRC", "species", "dataset", 
               "longitude", "latitude")

analysis_data_final <- analysis_data[,!(names(analysis_data) %in% drop_vars)]

# reorganize for for loops in later code
analysis_data_final <- analysis_data_final[, c("presence", "fold", names(analysis_data_final[c(1:26)]))]

# split dataset
analysis_data_final_AR <- analysis_data_final[,!(names(analysis_data_final) %in% "un_urban_gradient")]
analysis_data_final_UN <- analysis_data_final[,!(names(analysis_data_final) %in% "ar_urban_gradient")]

saveRDS(analysis_data_final_AR, "clean-data/tenagophila/tenagophila_AR_brt_data_oct12.rds")
saveRDS(analysis_data_final_UN, "clean-data/tenagophila/tenagophila_UN_brt_data_oct12.rds")


