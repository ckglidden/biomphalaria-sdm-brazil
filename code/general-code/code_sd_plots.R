############# plots for standard deviation of maps
library(raster)
library(RColorBrewer)
base_palette <- brewer.pal(9, name = "Greys")

##### read in sd tiffs
glabrata_1993 <- raster("/Users/cglidden/Documents/schisto_project/output/glabrata/aggregates/sd_brt_1990_glabrata_prediction.tif")
glabrata_2017 <- raster("/Users/cglidden/Documents/schisto_project/output/glabrata/aggregates/sd_brt_2017_glabrata_prediction.tif")

straminea_1993 <- raster("/Users/cglidden/Documents/schisto_project/output/straminea/unmasked/aggregates/sd_brt_1990_straminea_prediction.tif")
straminea_2017 <- raster("/Users/cglidden/Documents/schisto_project/output/straminea/unmasked/aggregates/sd_brt_2017_straminea_prediction.tif")

tenagophila_1993 <- raster("/Users/cglidden/Documents/schisto_project/output/tenagophila/unmasked/aggregates/sd_brt_1990_tenagophila_prediction.tif")
tenagophila_2017 <- raster("/Users/cglidden/Documents/schisto_project/output/tenagophila/unmasked/aggregates/sd_brt_2017_tenagophila_prediction.tif")

## plot
pdf("/Users/cglidden/Documents/schisto_project/output/all_snails/map_sds.pdf") 
par(mfrow = c(3,2), mar = c(0.5, 2, 0.5, 2), bty = 'n')
plot(glabrata_1993,
     main = "a. B. glabrata 1993",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(glabrata_2017,
     main = "b. B. glabrata 2017",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(straminea_1993,
     main = "c. B. straminea 1993",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(straminea_2017,
     main = "d. B. straminea 2017",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(tenagophila_1993,
     main = "e. B. tenagophila 1993",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

plot(tenagophila_2017,
     main = "f. B. tenagophila 2017",
     axes = F, frame.plot=FALSE,
     col = base_palette[2:9])

dev.off()