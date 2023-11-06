######### code to prepare dataframes for creating maps
library(raster)
library(sf)
library(ggplot2)
library(RColorBrewer)

##----------------------------------------------------##
## ENVIRONMENTAL DATA --------------------------------##
##----------------------------------------------------##

##########################################################
########## time invariant ##########
base_palette <- brewer.pal(9, name = "Purples")
  
water_recurrence <- raster("raw_data/env_rasters/base_data/waterRecur.tif")

upa <- raster("raw_data/env_rasters/base_data/upa.tif")
  
hnd <- raster("raw_data/env_rasters/base_data/hnd.tif")

dist_river <- raster("raw_data/env_rasters/base_data/riverDist.tif")

elevation <- raster("raw_data/env_rasters/base_data/elevation.tif")
  
aspect <- raster("raw_data/env_rasters/base_data/aspect.tif")

soil_clay <- raster("raw_data/env_rasters/base_data/clay.tif")
  
soil_sand <- raster("raw_data/env_rasters/base_data/sand.tif")
  
soil_water <- raster("raw_data/env_rasters/base_data/soilWater.tif")

soil_carbon <- raster("raw_data/env_rasters/base_data/soilCarbon.tif")
  
soil_ph <- raster("raw_data/env_rasters/base_data/pH.tif")

soil_density <- raster("raw_data/env_rasters/base_data/bulk.tif")

pdf("../output/all_snails/base_raster_supplement.pdf") 
par(mfrow = c(3,4), mar = c(0.5, 2, 0.5, 2), bty = 'n')
plot(water_recurrence,
     main = "a. water recurrence",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(log(upa + 1),
     main = "b. mean upstream
     drainage area (upa)",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(hnd,
     main = "c. height above
     nearest drainage (hnd)",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot((dist_river/1000),
     main = "d. distance to
     nearest river",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(elevation,
     main = "e. elevation",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(aspect,
     main = "f. aspect",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(soil_clay,
     main = "g. soil clay content",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(soil_sand,
     main = "h. soil sand content",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(soil_water,
     main = "i. soil water content",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(soil_carbon,
     main = "j. soil carbon content",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(soil_ph,
     main = "k. soil pH",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(soil_density,
     main = "l. soil bulk density",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])
dev.off()


##########################################################
########## land-use ##########
lulc_palette <- brewer.pal(9, name = "PuBuGn")
lulc_diff <- brewer.pal(9, name = "PRGn") 
  
ag_mosiac1 <- raster("raw_data/env_rasters/historical_data/ag_mosiac.tif")
ag_mosiac2 <- raster("raw_data/env_rasters/current_data/ag_mosiac.tif")
diff_ag_mosiac <- ag_mosiac2 - ag_mosiac1

temp_crops1 <- raster("raw_data/env_rasters/historical_data/temp_crops.tif")
temp_crops2 <- raster("raw_data/env_rasters/current_data/temp_crops.tif")
diff_temp_crops <- temp_crops2 - temp_crops1

total_population1 <- raster("raw_data/env_rasters/historical_data/total_population.tif")
total_population2 <- raster("raw_data/env_rasters/current_data/total_population.tif")
diff_total_population <- log(total_population2 + 1) - log(total_population1 + 1)

distance_int_pop1 <- raster("raw_data/env_rasters/historical_data/distance_int_pop.tif")
distance_int_pop2 <- raster("raw_data/env_rasters/current_data/distance_int_pop.tif")
diff_distance_int_pop <- distance_int_pop2 - distance_int_pop1

distance_high_pop1 <- raster("raw_data/env_rasters/historical_data/distance_high_pop.tif")
distance_high_pop2 <- raster("raw_data/env_rasters/current_data/distance_high_pop.tif")
diff_distance_high_pop <- distance_high_pop2 - distance_high_pop1

pdf("../output/all_snails/lulc_raster_supplement.pdf") 
par(mfrow = c(5, 3), mar = c(0.5, 2, 0.5, 2), bty = 'n')
# ag
plot(ag_mosiac1,
     main = "a. ag mosiac 1993",
     axes = F, frame.plot=FALSE,
     zlim = c(0,1),
     col = lulc_palette[2:9])

plot(ag_mosiac2,
     main = "b. ag mosiac 2017",
     axes = F, frame.plot=FALSE,
     zlim = c(0,1),
     col = lulc_palette[2:9])

plot(diff_ag_mosiac,
     main = "c. diff ag mosiac",
     axes = F, frame.plot=FALSE,
     col = lulc_diff)

# temp crops
plot(temp_crops1,
     main = "d. temp crops 1993",
     axes = F, frame.plot=FALSE,
     zlim = c(0,1),
     col = lulc_palette[2:9])

plot(temp_crops2,
     main = "e. temp crops 2017",
     axes = F, frame.plot=FALSE,
     zlim = c(0,1),
     col = lulc_palette[2:9])

plot(diff_temp_crops,
     main = "f. diff temp crops",
     axes = F, frame.plot=FALSE,
     col = lulc_diff)

# population
plot(log(total_population1 + 1),
     main = "g. log(population + 1)
     1993",
     zlim = c(0, 12),
     axes = F, frame.plot=FALSE,
     col = lulc_palette[2:9])

plot(log(total_population2 + 1),
     main = "h. log(population + 1)
     2017",
     axes = F, frame.plot=FALSE,
     zlim = c(0, 12),
     col = lulc_palette[2:9])

plot(diff_total_population,
     main = "i. diff log(population + 1)",
     axes = F, frame.plot=FALSE,
     col = lulc_diff[1:8])

# dist int population
plot((distance_int_pop1 / 1000),
     main = "j. dist urban area 
     (int density) 1993",
     axes = F, frame.plot=FALSE,
     col = lulc_palette[2:9])

plot((distance_int_pop2 / 1000),
     main = "k. dist urban area 
     (int density) 2017",
     axes = F, frame.plot=FALSE,
     col = lulc_palette[2:9])

plot((diff_distance_int_pop / 1000),
     main = "l. diff dist int pop",
     axes = F, frame.plot=FALSE,
     col = lulc_diff[1:8])

# dist high population
plot((distance_high_pop1 / 1000),
     main = "m. dist urban area 
     (int density) 1993",
     axes = F, frame.plot=FALSE,
     col = lulc_palette[2:9])

plot((distance_high_pop2 / 1000),
     main = "n. dist urban area 
     (int density) 2017",
     axes = F, frame.plot=FALSE,
     col = lulc_palette[2:9])

plot((diff_distance_high_pop / 1000),
     main = "o. diff dist int pop",
     axes = F, frame.plot=FALSE,
     col = lulc_diff[1:8])
dev.off()


##########################################################
########## precipitation ##########
precip_palette <- brewer.pal(9, name = "Blues")
precip_diff <- brewer.pal(9, name = "BrBG") 

# bio12
annual_precip1 <- raster("raw_data/env_rasters/historical_data/bio12.tif")
annual_precip2 <- raster("raw_data/env_rasters/current_data/bio12.tif")
diff_annual_precip <- annual_precip2 - annual_precip1

# bio13
precip_wet_mnth1 <- raster("raw_data/env_rasters/historical_data/bio13.tif")
precip_wet_mnth2 <- raster("raw_data/env_rasters/current_data/bio13.tif")
diff_precip_wet_mnth <- precip_wet_mnth2 - precip_wet_mnth1

# bio14
precip_dry_mnth1 <- raster("raw_data/env_rasters/historical_data/bio14.tif")
precip_dry_mnth2 <- raster("raw_data/env_rasters/current_data/bio14.tif")
diff_precip_dry_mnth <- precip_dry_mnth2 - precip_dry_mnth1

# bio15
precip_seasonality1 <- raster("raw_data/env_rasters/historical_data/bio15.tif")
precip_seasonality2 <- raster("raw_data/env_rasters/current_data/bio15.tif")
diff_precip_seasonality <- precip_seasonality2 - precip_seasonality1

# bio16
precip_wet_qt1 <- raster("raw_data/env_rasters/historical_data/bio16.tif")
precip_wet_qt2 <- raster("raw_data/env_rasters/current_data/bio16.tif")
diff_precip_wet_qt <- precip_wet_qt2 - precip_wet_qt1

pdf("../output/all_snails/precip_raster_supplement.pdf") 
par(mfrow = c(5, 3), mar = c(0.5, 2, 0.5, 2), bty = 'n')

# bio12
plot(annual_precip1,
     main = "a. annual precip 1990s",
     axes = F, frame.plot=FALSE,
     zlim = c(30000, 70000),
     col = precip_palette)

plot(annual_precip2,
     main = "b. annual precip 2010s",
     axes = F, frame.plot=FALSE,
     zlim = c(30000, 70000),
     col = precip_palette)

plot(diff_annual_precip,
     main = "c. diff annual precip",
     axes = F, frame.plot=FALSE,
     col = precip_diff[1:6])

# bio13
plot(precip_wet_mnth1,
     main = "d. precip wettest 
     month 1990s",
     axes = F, frame.plot=FALSE,
     zlim = c(3500, 70000),
     col = precip_palette)

plot(precip_wet_mnth2,
     main = "e. precip wettest 
     month 2010s",
     axes = F, frame.plot=FALSE,
    zlim = c(3500, 70000),
     col = precip_palette)

plot(diff_precip_wet_mnth,
     main = "f. diff precip
     wettest month",
     axes = F, frame.plot=FALSE,
     col = precip_diff)

# bio14
plot(precip_dry_mnth1,
     main = "d. precip driest 
     month 1990s",
     axes = F, frame.plot=FALSE,
     zlim = c(0, 25000),
     col = precip_palette)

plot(precip_dry_mnth2,
     main = "e. precip driest 
     month 2010s",
     axes = F, frame.plot=FALSE,
      zlim = c(0, 25000),
     col = precip_palette)

plot(diff_precip_dry_mnth,
     main = "f. diff precip
     driest month",
     axes = F, frame.plot=FALSE,
     col = precip_diff[1:7])

# bio15
plot(precip_seasonality1,
     main = "d. precip_seasonality 1990s",
     axes = F, frame.plot=FALSE,
     zlim = c(0, 180),
     col = precip_palette)

plot(precip_seasonality2,
     main = "e. precip_seasonality",
     axes = F, frame.plot=FALSE,
     zlim = c(0, 180),
     col = precip_palette)

plot(diff_precip_seasonality,
     main = "f. diff precip_seasonality",
     axes = F, frame.plot=FALSE,
     col = precip_diff[1:9])

# bio16
plot(precip_wet_qt1,
     main = "d. precip wettest 
     quarter 1990s",
     axes = F, frame.plot=FALSE,
     zlim = c(10000, 75000),
     col = precip_palette)

plot(precip_wet_qt2,
     main = "e. precip wettest 
     quarter 2010s",
     axes = F, frame.plot=FALSE,
     zlim = c(10000, 75000),
     col = precip_palette)

plot(diff_precip_wet_qt,
     main = "f. diff precip
     wettest quarter",
     axes = F, frame.plot=FALSE,
     col = precip_diff)
dev.off()

##########################################################
########## temp ##########
temp_palette <- brewer.pal(9, name = "YlOrRd")
temp_diff <- brewer.pal(9, name = "RdYlBu") 

# bio2
diurn_temp1 <- raster("raw_data/env_rasters/historical_data/bio2.tif")
diurn_temp2 <- raster("raw_data/env_rasters/current_data/bio2.tif")
diff_diurn_temp <- diurn_temp2 - diurn_temp1

# bio3
isothermality1 <- raster("raw_data/env_rasters/historical_data/bio3.tif")
isothermality2 <- raster("raw_data/env_rasters/current_data/bio3.tif")
diff_isothermality <- isothermality2 - isothermality1

# bio8
temp_wet_qt1 <- raster("raw_data/env_rasters/historical_data/bio8.tif")
temp_wet_qt2 <- raster("raw_data/env_rasters/current_data/bio8.tif")
diff_temp_wet_qt <- ((temp_wet_qt2 - 273.15)*0.01) - ((temp_wet_qt1 - 273.15)*0.01)

# bio9
temp_dry_qt1 <- raster("raw_data/env_rasters/historical_data/bio9.tif")
temp_dry_qt2 <- raster("raw_data/env_rasters/current_data/bio9.tif")
diff_temp_dry_qt <- ((temp_dry_qt2 - 273.15)*0.01) - ((temp_dry_qt1 - 273.15)*0.01)


####### temp figure
pdf("../output/all_snails/temp_raster_supplement.pdf") 
par(mfrow = c(4, 3), mar = c(0.5, 2, 0.5, 2), bty = 'n')

# bio2
plot(diurn_temp1,
     main = "a. diurnal temp range 1990s",
     axes = F, frame.plot=FALSE,
     col = temp_palette)

plot(diurn_temp2,
     main = "b. diurnal temp range 2010s",
     axes = F, frame.plot=FALSE,
     col = temp_palette)

plot(diff_diurn_temp,
     main = "c. diff temp range",
     axes = F, frame.plot=FALSE,
     col = temp_diff[2:9])

# bio3
plot(isothermality1,
     main = "d. isothermality 1990s",
     axes = F, frame.plot=FALSE,
     zlim = c(0, 80),
     col = temp_palette)

plot(isothermality2,
     main = "e. isothermality 2010s",
     axes = F, frame.plot=FALSE,
     zlim = c(0, 80),
     col = temp_palette)

plot(diff_isothermality,
     main = "f. diff isothermality",
     axes = F, frame.plot=FALSE,
     col = temp_diff[1:7])

# bio8
plot((temp_wet_qt1 - 273.15)*0.01,
     main = "g. temp wettest
     quarter 1990s",
     axes = F, frame.plot=FALSE,
     zlim = c(25.5, 27.5),
     col = temp_palette)

plot((temp_wet_qt2 - 273.15)*0.01,
     main = "h. temp wettest
     quarter 2010s",
     axes = F, frame.plot=FALSE,
     zlim = c(25.5, 27.5),
     col = temp_palette)

plot(diff_temp_wet_qt,
     main = "i. diff temp
     wettest quarter",
     axes = F, frame.plot=FALSE,
     col = temp_diff)

# bio9
plot((temp_dry_qt1 - 273.15)*0.01,
     main = "g. temp driest
     quarter 1990s",
     axes = F, frame.plot=FALSE,
     zlim = c(25, 28),
     col = temp_palette)

plot((temp_dry_qt2 - 273.15)*0.01,
     main = "h. temp driest
     quarter 2010s",
     axes = F, frame.plot=FALSE,
     zlim = c(25, 28),
     col = temp_palette)

plot(diff_temp_wet_qt,
     main = "i. diff temp
     driest quarter",
     axes = F, frame.plot=FALSE,
     col = temp_diff)
dev.off()

##----------------------------------------------------##
## SNAIL DATA ----------------------------------------##
##----------------------------------------------------##

## ** NOTE: need to read in cleaned data for this ** ##



#---------------------------------------------------------------------------------------------------------
# Figure 1: Presence points by species, (a) national, (b) sudeste, (c) sp/mg
#---------------------------------------------------------------------------------------------------------
library(geobr)
library(ggplot2)
library(sf)
library(dplyr)
library(ggpubr)
library(gridExtra)
library(cowplot)
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

states <- read_state(
  year=2020,
  showProgress = FALSE)

gb <- readRDS("../clean data/all_data_sdm_april7.rds") %>% 
  filter(grepl("glabrata", species, ignore.case = TRUE)) %>%
  select(longitude, latitude) %>%
  mutate(species = "B. glabrata")
  
gs <- readRDS("../clean data/all_data_sdm_april7.rds") %>% 
  filter(grepl("straminea", species, ignore.case = TRUE)) %>%
  select(longitude, latitude) %>%
  mutate(species = "B. straminea")

gt <- readRDS("../clean data/all_data_sdm_april7.rds") %>% 
  filter(grepl("tenagophila", species, ignore.case = TRUE)) %>%
  select(longitude, latitude) %>%
  mutate(species = "B. tenagophila")

all_hosts <- rbind(gb, gs, gt)
host_coords <- st_as_sf(all_hosts, coords = c(1:2))
st_crs(host_coords) <- 4326

# Plot all Brazilian states
point_fig <- ggplot() +
  geom_sf(data=states, fill="#F4F4F4", color="#626262", size=.15, show.legend = FALSE) +
          geom_sf(data = host_coords, aes(geometry = geometry, color=species), size = 2, alpha=0.25) +
            scale_color_manual(values=c("#C24425","#25CED1", "#2D936C")) +
                                          theme_minimal() +
                                          no_axis +
                                          xlim(c(-72.5, -35))

ggsave("../output/all_snails/occurrence_points.pdf", point_fig, dpi = 600) 
ggsave("../output/all_snails/occurrence_points.png", point_fig, dpi = 600) 

















