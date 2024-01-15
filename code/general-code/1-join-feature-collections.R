### This code cleans an merges all feature collections so that GEE data can be downloaded simultaneously

#--------------------------------#
# set working directory          #
#--------------------------------#
setwd("biomphalaria-sdm-brazil") # set path based on where you save it on local machine

#-------------------------#
# load libraries          #
#-------------------------#

library(sf); library(rgdal); library(dplyr); library(tidyr)

#-----------------------------------------------#
# bkg points: all aquatic gbif animals          #
#-----------------------------------------------#

all_aquatic <- st_read("raw_data/feature-collections/all_aquatic/aquatic_species_occ_pts")

all_aquatic_sub <- all_aquatic[, c("scntfcN", "year", "row_code", "geometry")]
all_aquatic_sub$source <- "gbif"
all_aquatic_sub$dataset <- "all_aquatic"

names(all_aquatic_sub) <- c("species", "year", "origRC", "geometry", "source", "dataset")

#-----------------------------------------------#
# bkg points: fiocruz non competent species     #
#-----------------------------------------------#
fio_bkg <- st_read("raw_data/feature_collections/fio_snails_bra_other_v1")
fio_bkg_sub <- fio_bkg[, c("species", "year", "col_code", "geometry")]
fio_bkg_sub$source <- "fio"
fio_bkg_sub$dataset <- "fio_snails_bra_other_v1"

names(fio_bkg_sub) <- c("species", "year", "origRC", "geometry", "source", "dataset")

#-----------------------------------------#
# pres points: fiocruz host species       #
#-----------------------------------------#
fio_snails <- st_read("raw_data/feature_collections/fio_snails_bra_v2")
fio_snails_sub <- fio_snails[, c("species", "year", "col_code", "geometry")]
fio_snails_sub$source <- "fio"
fio_snails_sub$dataset <- "fio_snails_bra_v2"

names(fio_snails_sub) <- c("species", "year", "origRC", "geometry", "source", "dataset")

#---------------------------#
# Sao Paulo snails          #
#---------------------------#
sp_snails <- st_read("raw_data/feature_collections/sp_snails")
sp_snails <- subset(sp_snails, presenc == 1)
sp_snails_sub <- sp_snails[, c("species", "year", "row_id", "geometry")]
sp_snails_sub$source <- "raquel"
sp_snails_sub$dataset <- "sp_snails"

names(sp_snails_sub) <- c("species", "year", "origRC", "geometry", "source", "dataset")

#crs currently NA, need to change it to the same as other datasets
sp_snails_sub <- sp_snails_sub %>% st_set_crs(st_crs(all_aquatic_sub))

#-----------------------------------------------------#
# combine all datasets & save for GEE upload          #
#-----------------------------------------------------#
all_data <- rbind(all_aquatic_sub, fio_bkg_sub, fio_snails_sub, sp_snails_sub)
all_data$row_code <- seq(1, nrow(all_data), by = 1)

write_sf(all_data, "raw-data/feature-collections/all_points_schisto_sdm.shp")










