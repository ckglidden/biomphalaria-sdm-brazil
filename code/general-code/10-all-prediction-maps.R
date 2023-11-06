### this code creates prediction maps for schisto paper

setwd("/Users/carolineglidden/Documents/shisto-project")
library(raster)
library(RColorBrewer)
library(viridis)
library(paletteer)
library(geobr)
library(ggplot2)

##----------------------------##
## set up colors.             ##
##----------------------------##

#palettes
mean_pal <- viridis(9, option = "D", direction = -1)[2:9]
sd_pal <- viridis(7, option = "A", direction = -1)

# pal <- c("grey93", "grey93", pal0[2:7])
difference_pal0<-  paletteer_c("ggthemes::Red-Blue Diverging", 20)
difference_pal <- c(difference_pal0[1:8], "grey70", "grey70", difference_pal0[13:20])

#pal3 <- viridis::viridis(10, option = "C") 

##---------------------------------##
## historical distribution (1992)  ##
##---------------------------------##

# glabrata
glabrata_hist_mean <- raster("glabrata/summarized-geoTiffs/glabrata_predicted_mean_distribution_1992.tif")
glabrata_hist_sd <- raster("glabrata/summarized-geoTiffs/glabrata_standard_dev_predicted_distribution_1992.tif")

# straminea
straminea_hist_mean <- raster("straminea/summarized-geoTiffs/straminea_predicted_mean_distribution_1992.tif")
straminea_hist_sd <- raster("straminea/summarized-geoTiffs/straminea_standard_dev_predicted_distribution_1992.tif")

# tenagophila
ten_hist_mean <- raster("tenagophila/summarized-geoTiffs/tenagophila_predicted_mean_distribution_1992.tif")
ten_hist_sd <- raster("tenagophila/summarized-geoTiffs/tenagophila_standard_dev_predicted_distribution_1992.tif")


##----------------------------##
## current distribution.      ##
##----------------------------##

# glabrata
glabrata_current_mean <- raster("glabrata/summarized-geoTiffs/glabrata_predicted_mean_distribution_2017.tif")
glabrata_current_sd <-  raster("glabrata/summarized-geoTiffs/glabrata_standard_dev_predicted_distribution_2017.tif")

# straminea
straminea_current_mean <- raster("straminea/summarized-geoTiffs/straminea_predicted_mean_distribution_2017.tif")
straminea_current_sd <-  raster("straminea/summarized-geoTiffs/straminea_standard_dev_predicted_distribution_2017.tif")

# tenagophila
ten_current_mean <- raster("tenagophila/summarized-geoTiffs/tenagophila_predicted_mean_distribution_2017.tif")
ten_current_sd <-  raster("tenagophila/summarized-geoTiffs/tenagophila_standard_dev_predicted_distribution_2017.tif")


##----------------------------##
## climate counterfactual     ##
##----------------------------##

# glabrata
glabrata_climate_mean <- raster("glabrata/summarized-geoTiffs/glabrata_predicted_mean_distribution_climateCF.tif")
glabrata_climate_sd <- raster("glabrata/summarized-geoTiffs/glabrata_standard_dev_predicted_distribution_climateCF.tif")

# straminea
straminea_climate_mean <- raster("straminea/summarized-geoTiffs/straminea_predicted_mean_distribution_climateCF.tif")
straminea_climate_sd <- raster("straminea/summarized-geoTiffs/straminea_standard_dev_predicted_distribution_climateCF.tif")

# tenagophila
ten_climate_mean <- raster("tenagophila/summarized-geoTiffs/tenagophila_predicted_mean_distribution_climateCF.tif")
ten_climate_sd <- raster("tenagophila/summarized-geoTiffs/tenagophila_standard_dev_predicted_distribution_climateCF.tif")


##--------------------------##
## urban counterfactual     ##
##--------------------------##

# glabrata
glabrata_urban_mean <- raster("glabrata/summarized-geoTiffs/glabrata_predicted_mean_distribution_urbanCF.tif")
glabrata_urban_sd <- raster("glabrata/summarized-geoTiffs/glabrata_standard_dev_predicted_distribution_urbanCF.tif")

# straminea
straminea_urban_mean <- raster("straminea/summarized-geoTiffs/straminea_predicted_mean_distribution_urbanCF.tif")
straminea_urban_sd <- raster("straminea/summarized-geoTiffs/straminea_standard_dev_predicted_distribution_urbanCF.tif")

# tenagophila
ten_urban_mean <- raster("tenagophila/summarized-geoTiffs/tenagophila_predicted_mean_distribution_urbanCF.tif")
ten_urban_sd <- raster("tenagophila/summarized-geoTiffs/tenagophila_standard_dev_predicted_distribution_urbanCF.tif")

##------------------------------##
## change over time national.   ##
##------------------------------##

# mean max transformation for 1992 & 2017 maps to better look at difference

## glabrata
#### hist glabrata min = 0.1885575 & max = 0.7725099
#### current glabrata min = 0.188547 & max = 0.7727576
glabrata_hist_transform <- (glabrata_hist_mean - 0.188547) / (0.7727576 - 0.188547) 
glabrata_current_transform <- (glabrata_current_mean - 0.188547) / (0.7727576 - 0.188547) 

## straminea
#### hist straminea min = 0.1226853 & max = 0.8456495
#### current straminea min = 0.1229473 & max = 0.8447081
straminea_hist_transform <- (straminea_hist_mean - 0.1226853) / (0.8456495 - 0.1226853) 
straminea_current_transform <- (straminea_current_mean - 0.1226853) / (0.8456495 - 0.1226853) 

## tenagophila
#### hist tenagophila min = 0.07091966 & max = 0.9266439
#### current tenagophila min = 0.0709205 & max = 0.9278836
tenagophila_hist_transform <- (ten_hist_mean - 0.07091966) / (0.9278836 - 0.07091966) 
tenagophila_current_transform <- (ten_current_mean - 0.07091966) / (0.9278836 - 0.07091966) 

pdf('general-code/all-snails-observed-predictions.pdf')
par(mfrow=c(3, 3), bty = 'n') ## change box to frame plot

# glabrata
plot(glabrata_hist_transform, # update this to min max transformation
     zlim = c(0, 1), 
     col = mean_pal,
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

plot(glabrata_current_transform, # update this to min max transformation
     zlim = c(0, 1), 
     col = mean_pal,
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE,)

plot(glabrata_current_transform - glabrata_hist_transform, 
     col = difference_pal, 
     zlim = c(-0.9, 0.9),
     axes = FALSE, frame.plot = FALSE)
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

# straminea
plot(straminea_hist_transform, # update this to min max transformation
     zlim = c(0, 1), 
     col = mean_pal,
     axes = FALSE, frame.plot=FALSE,
     legend = FALSE)

plot(straminea_current_transform, # update this to min max transformation
     zlim = c(0, 1), 
     col = mean_pal,
     axes = FALSE, frame.plot = FALSE)

plot(straminea_current_transform - straminea_hist_transform, 
     col = difference_pal,
     zlim = c(-0.9, 0.9),
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

# tenagophila
plot(tenagophila_hist_transform, # update this to min max transformation
     zlim = c(0, 1), 
     col = mean_pal,
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

plot(tenagophila_current_transform, # update this to min max transformation
     zlim = c(0, 1), 
     col = mean_pal,
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

plot(tenagophila_current_transform - tenagophila_hist_transform, 
     col = difference_pal,
     zlim = c(-0.9, 0.9),
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

dev.off()

##------------------------------##
## national counter-factuals    ##
##------------------------------##

# mean max transformation for 1992 & 2017 maps to better look at difference

## glabrata
#### current glabrata min = 0.188547 & max = 0.7727576
#### climate glabrata min = 0.1885575 & max = 0.7653399
#### urban glabrata min = 0.188547 & max = 0.774653
glabrata_current_transform <- (glabrata_current_mean - 0.188547) / (0.774653 - 0.188547) 
glabrata_climate_transform <- (glabrata_climate_mean - 0.188547) / (0.774653 - 0.188547); glabrata_climate_change <- glabrata_current_transform - glabrata_climate_transform 
glabrata_urban_transform <- (glabrata_urban_mean - 0.188547) / (0.774653 - 0.188547); glabrata_urban_change <- glabrata_current_transform - glabrata_urban_transform  
writeRaster(glabrata_urban_change, "glabrata/summarized-geoTiffs/glabrata_predicted_urbanCF_attribution.tif", overwrite = TRUE)

## straminea
#### current straminea min = 0.1229473 & max = 0.8447081
#### climate straminea min = 0.1226853 & max = 0.8435981
#### urban straminea min = 0.1229473 & max = 0.8467516
straminea_current_transform <- (straminea_current_mean - 0.1226853) / (0.8435981 - 0.1226853) 
straminea_climate_transform <- (straminea_climate_mean - 0.1226853) / (0.8435981 - 0.1226853); straminea_climate_change <- straminea_current_transform - straminea_climate_transform 
straminea_urban_transform <- (straminea_urban_mean - 0.1226853) / (0.8435981 - 0.1226853); straminea_urban_change <- straminea_current_transform - straminea_urban_transform
writeRaster(straminea_urban_change, "straminea/summarized-geoTiffs/straminea_predicted_urbanCF_attribution.tif", overwrite = TRUE)

## tenagophila
#### current tenagophila min = 0.0709205 & max = 0.9278836
#### climate tenagophila min = 0.07091966 & max = 0.9266439
#### urban tenagophila min = 0.0709205 & max = 0.9273519
tenagophila_current_transform <- (ten_current_mean - 0.0709205) / (0.9278836 - 0.0709205) 
tenagophila_climate_transform <- (ten_climate_mean - 0.0709205) / (0.9278836 - 0.0709205); tenagophila_climate_change <- tenagophila_current_transform - tenagophila_climate_transform 
tenagophila_urban_transform <- (ten_urban_mean - 0.0709205) / (0.9278836 - 0.0709205); tenagophila_urban_change <- tenagophila_current_transform - tenagophila_urban_transform
writeRaster(tenagophila_urban_change, "tenagophila/summarized-geoTiffs/tenagophila_predicted_urbanCF_attribution.tif", overwrite = TRUE)

pdf('general-code/all-snails-counterfactuals.pdf')
par(mfrow = c(3, 2), bty = 'n')
# glabrata
## climate counter-factual
plot(glabrata_climate_change, 
     col = difference_pal,
     zlim = c(-0.8, 0.8),
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

## urban counter-factual
plot(glabrata_urban_change, 
     col = difference_pal,
     zlim = c(-0.8, 0.8),
     axes = FALSE, frame.plot = FALSE)

# straminea
## climate counter-factual
plot(straminea_climate_change, 
     col = difference_pal,
     zlim = c(-0.8, 0.8),
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

## urban counter-factual
plot(straminea_urban_change, 
     col = difference_pal,
     zlim = c(-0.8, 0.8),
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

# tenagophila
## climate counter-factual
plot(tenagophila_climate_change, 
     col = difference_pal,
     zlim = c(-0.8, 0.8),
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

## urban counter-factual
plot(tenagophila_urban_change, 
     col = difference_pal,
     zlim = c(-0.8, 0.8),
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)
scalebar(500,
         below = 'km',
         xy = c(-45, -30))
dev.off()

##----------------------##
## regional figures     ##
##----------------------##

belo_urban_1992 <- raster("general-code/regional-summarized-tiffs/contig_urban_area_1992_belo.tif"); belo_urban_1992[belo_urban_1992 == 4] <- 1; belo_urban_1992[belo_urban_1992 == 3] <- 2
belo_urban_2017 <- raster("general-code/regional-summarized-tiffs/contig_urban_area_2017_belo.tif"); belo_urban_2017[belo_urban_2017 == 4] <- 1; belo_urban_2017[belo_urban_2017 == 3] <- 2

sp_urban_1992 <- raster("general-code/regional-summarized-tiffs/contig_urban_area_1992_sp.tif"); sp_urban_1992[sp_urban_1992 == 4] <- 1; sp_urban_1992[sp_urban_1992 == 3] <- 2
sp_urban_2017 <- raster("general-code/regional-summarized-tiffs/contig_urban_area_2017_sp.tif"); sp_urban_2017[sp_urban_2017 == 4] <- 1; sp_urban_2017[sp_urban_2017== 3] <- 2


# Minas Gerais - Belo Horizonte microregion
glabrata_belo_change <- raster('glabrata/summarized-geoTiffs/glabrata_urban_change_belo.tif'); plot(glabrata_belo_change)
straminea_belo_change <- raster('straminea/summarized-geoTiffs/straminea_urban_change_belo.tif'); plot(straminea_belo_change)

# Sao Paulo - Sao Paulo microregion 
tenagophila_sp_change <- raster('tenagophila/summarized-geoTiffs/tenagophila_urban_change_sp.tif'); plot(tenagophila_sp_change)

# put plot together
pdf('general-code/all-snails-regional-urbanCF.pdf')
par(mfrow = c(2, 4), bty = 'n')

plot.new()

plot(sp_urban_1992,
     col = paletteer_d("RColorBrewer::Purples", 9, direction = 1, type = "discrete")[c(3, 6, 9)], # figure out raster plot discrete colors
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

plot(sp_urban_2017,
     col = paletteer_d("RColorBrewer::Purples", 9, direction = 1, type = "discrete")[c(3, 6, 9)], # figure out raster plot discrete colors
     axes = FALSE, frame.plot = FALSE)

plot(tenagophila_sp_change,
     col = difference_pal,
     zlim = c(-0.8, 0.8), 
     axes = FALSE, frame.plot = FALSE,
     legend = TRUE)
scalebar(10,
         below = 'km',
         xy = c(-46.5, -24))

plot(belo_urban_1992,
     col = paletteer_d("RColorBrewer::Purples", 9, direction = 1, type = "discrete")[c(3, 6, 9)], # figure out raster plot discrete colors
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

plot(belo_urban_2017,
     col = paletteer_d("RColorBrewer::Purples", 9, direction = 1, type = "discrete")[c(3, 6, 9)], # figure out raster plot discrete colors
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

plot(glabrata_belo_change,
     col = difference_pal,
     zlim = c(-0.8, 0.8),
      axes = FALSE, frame.plot = FALSE,
     legend = FALSE)

plot(straminea_belo_change,
     col = difference_pal,
     zlim = c(-0.8, 0.8),
     axes = FALSE, frame.plot = FALSE,
     legend = FALSE)
scalebar(10,
         below = 'km',
         xy = c(-44, -20.5))
dev.off()

##---------------------##
## map Brazil.         ##
##---------------------##

brazil <- read_country(year = 2018)

# minas gerais microregion

belo_micro <- read_micro_region(code_micro = 31030, year = "2018")

# sao paulo microregion

sao_micro <- read_micro_region(code_micro = 35061, year = "2018")

map_figure <- ggplot() +
  geom_sf(data = brazil, col = 'black') + 
  geom_sf(data = belo_micro, col = 'black', fill = 'magenta4') + 
  geom_sf(data = sao_micro, col = 'black', fill = 'blue3') + theme_void()


micro_figure <- ggplot() +
  geom_sf(data = belo_micro, col = 'black', fill = 'magenta4') + 
  geom_sf(data = sao_micro, col = 'black', fill = 'blue3') + theme_void()

ggsave('general-code/brazil-geo.pdf', gridExtra::arrangeGrob(map_figure, micro_figure))


##---------------------##
## standard deviation  ##
##---------------------##

pdf('general-code/all-snails-sd-predictions.pdf')
par(mfrow=c(3, 2), bty = 'n')
# glabrata
plot(glabrata_hist_sd, 
     col = sd_pal,
     axes = FALSE,
     main = substitute(paste(italic('B. glabrata'), " 1992")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

plot(glabrata_current_sd, 
     col = sd_pal,
     axes = FALSE,
     main = substitute(paste(italic('B. glabrata'), " 2017")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

# plot(glabrata_climate_sd, 
#      col = sd_pal,
#      axes = FALSE,
#      main = substitute(paste(italic('B. glabrata'), " climate counterfactual")))
# scalebar(500,
#          below = 'km',
#          xy = c(-45, -30))
# 
# plot(glabrata_urban_sd, 
#      col = sd_pal,
#      axes = FALSE,
#      main = substitute(paste(italic('B. glabrata'), " urban counterfactual")))
# scalebar(500,
#          below = 'km',
#          xy = c(-45, -30))

# straminea
plot(straminea_hist_sd, 
     col = sd_pal,
     axes = FALSE,
     main = substitute(paste(italic('B. straminea'), " 1992")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

plot(straminea_current_sd, 
     col = sd_pal,
     axes = FALSE,
     main = substitute(paste(italic('B. straminea'), " 2017")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))
# 
# plot(straminea_climate_sd, 
#      col = sd_pal,
#      axes = FALSE,
#      main = substitute(paste(italic('B. straminea'), 
#                              " climate counterfactual")))
# scalebar(500,
#          below = 'km',
#          xy = c(-45, -30))
# 
# plot(straminea_urban_sd, 
#      col = sd_pal,
#      axes = FALSE,
#      main = substitute(paste(italic('B. straminea'), 
#                              " urban counterfactual")))
# scalebar(500,
#          below = 'km',
#          xy = c(-45, -30))

# tenagophila
plot(ten_hist_sd, 
     col = sd_pal,
     axes = FALSE,
     main = substitute(paste(italic('B. tenagophila'), " 1992")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

plot(ten_current_sd, 
     col = sd_pal,
     axes = FALSE,
     main = substitute(paste(italic('B. tenagophila'), " 2017")))
scalebar(500,
         below = 'km',
         xy = c(-45, -30))

# plot(ten_climate_sd, 
#      col = sd_pal,
#      axes = FALSE,
#      main = substitute(paste(italic('B. tenagophila'), 
#                              " climate counterfactual")))
# scalebar(500,
#          below = 'km',
#          xy = c(-45, -30))
# 
# plot(ten_urban_sd, 
#      col = sd_pal,
#      axes = FALSE,
#      main = substitute(paste(italic('B. tenagophila'), 
#                              " urban counterfactual")))
# scalebar(500,
#          below = 'km',
#          xy = c(-45, -30))
dev.off()














