#carterquesenberry final project
library(terra)
library(ggplot2)


#add landsat 7 (1999) data:
landsat7_blue <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LE07_L1TP_039035_19991008_20200918_02_T1/LE07_L1TP_039035_19991008_20200918_02_T1_B1.TIF")
landsat7_green <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LE07_L1TP_039035_19991008_20200918_02_T1/LE07_L1TP_039035_19991008_20200918_02_T1_B2.TIF")
landsat7_red <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LE07_L1TP_039035_19991008_20200918_02_T1/LE07_L1TP_039035_19991008_20200918_02_T1_B3.TIF")
landsat7_nir <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LE07_L1TP_039035_19991008_20200918_02_T1/LE07_L1TP_039035_19991008_20200918_02_T1_B4.TIF")
landsat7_swir <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LE07_L1TP_039035_19991008_20200918_02_T1/LE07_L1TP_039035_19991008_20200918_02_T1_B5.TIF")

#combine all bands:
landsat7 <- c(landsat7_blue, landsat7_green, landsat7_red, landsat7_nir, landsat7_swir)
#reproject to NAD83 crs:
landsat7 <- project(landsat7, "EPSG:4269")

#add landsat 9 (2023) data:
landsat9_blue <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LC09_L1TP_039035_20231213_20231213_02_T1/LC09_L1TP_039035_20231213_20231213_02_T1_B2.TIF")
landsat9_green<- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LC09_L1TP_039035_20231213_20231213_02_T1/LC09_L1TP_039035_20231213_20231213_02_T1_B3.TIF")
landsat9_red <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LC09_L1TP_039035_20231213_20231213_02_T1/LC09_L1TP_039035_20231213_20231213_02_T1_B4.TIF")
landsat9_nir <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LC09_L1TP_039035_20231213_20231213_02_T1/LC09_L1TP_039035_20231213_20231213_02_T1_B5.TIF")
landsat9_swir1 <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LC09_L1TP_039035_20231213_20231213_02_T1/LC09_L1TP_039035_20231213_20231213_02_T1_B6.TIF")
landsat9_swir2 <- rast("/Users/carterqberry/R Studio/Final Project/Landsat/LC09_L1TP_039035_20231213_20231213_02_T1/LC09_L1TP_039035_20231213_20231213_02_T1_B7.TIF")

#combine all bands:
landsat9 <- c(landsat9_blue, landsat9_green, landsat9_red, landsat9_nir, landsat9_swir1, landsat9_swir2)
#reproject to NAD83 crs:
landsat9 <- project(landsat9, "EPSG:4269")

#set las vegas extent:
vegas_extent <- as.polygons(ext(c(-115.4, -114.95, 36, 36.32)))
crs(vegas_extent) <- "EPSG:4269"
vegas_extent <- project(vegas_extent, "EPSG:4269")

#crop landsat 7 to vegas extent:
landsat7_vegas <- crop(landsat7, vegas_extent)
plotRGB(landsat7_vegas, r=3, g=2, b=1, stretch = "linear", main="Landsat 7 Image of Las Vegas", mar = 2)

#crop landsat 9 to vegas extent:
landsat9_vegas <- crop(landsat9, vegas_extent)
plotRGB(landsat9_vegas, r=3, g=2, b=1, stretch = "linear", main="Landsat 9 Image of Las Vegas", mar = 2)

#set lake mead extent:
mead_extent <- as.polygons(ext(c(-114.95, -113.85, 35.9, 36.6)))
crs(mead_extent) <- "EPSG:4269"
mead_extent <- project(mead_extent, "EPSG:4269")

#crop landsat 7 to mead extent:
landsat7_mead <- crop(landsat7, mead_extent)
plotRGB(landsat7_mead, r=3, g=2, b=1, stretch = "linear", main="Landsat 7 Image of Lake Mead", mar = 2)

#crop landsat 9 to mead extent:
landsat9_mead <- crop(landsat9, mead_extent)
plotRGB(landsat9_mead, r=3, g=2, b=1, stretch = "linear", main="Landsat 9 Image of Lake Mead", mar = 2)

#landsat 7 ndbi:
landsat7_ndbi <- (landsat7_vegas[[5]]-landsat7_vegas[[4]]) / (landsat7_vegas[[5]]+landsat7_vegas[[4]])
landsat7_ndbi <- landsat7_ndbi < 0.05
plot(landsat7_ndbi, col = c("transparent", "orange"), legend = FALSE)
legend("topleft", legend = "Built Area", fill = "orange", cex = .6)


#landsat 9 ndbi:
landsat9_ndbi <- (landsat9_vegas[[5]]-landsat9_vegas[[4]]) / (landsat9_vegas[[5]]+landsat9_vegas[[4]])
landsat9_ndbi <- landsat9_ndbi < 0
plot(landsat9_ndbi, col = c("transparent","orange"), legend = FALSE)
legend("topleft", legend = "Built Area", fill = "orange", cex = .6)

#landsat 7 mndwi:
landsat7_mndwi <- (landsat7_mead[[2]]-landsat7_mead[[5]]) / (landsat7_mead[[2]]+landsat7_mead[[5]])
landsat7_mndwi <- landsat7_mndwi > 0.2
plot(landsat7_mndwi, col = c("transparent","#97f5ff"), legend = FALSE)
legend("topleft", legend = "Water", fill = "#97f5ff", cex = .6)


#landsat 9 mndwi:
landsat9_mndwi <- (landsat9_mead[[2]]-landsat9_mead[[5]]) / (landsat9_mead[[2]]+landsat9_mead[[5]])
landsat9_mndwi <- landsat9_mndwi > 0.1
plot(landsat9_mndwi, col = c("transparent","#97f5ff"), legend = FALSE)
legend("topleft", legend = "Water", fill = "#97f5ff", cex = .6)

#match the extents:
landsat7_mndwi <- resample(landsat7_mndwi, landsat9_mndwi)
landsat7_ndbi <- resample(landsat7_ndbi, landsat9_ndbi)

#create difference maps:
mead_diff <- landsat9_mndwi - landsat7_mndwi
plot(mead_diff, col = c("red", "transparent"), main = "1999-2024 Lake Mead Water Level Changes")
legend("topleft", legend = "Difference", fill = "red", cex = .7)

vegas_diff <- landsat7_ndbi - landsat9_ndbi
plot(vegas_diff, col = c("red", "transparent"), main = "1999-2024 Las Vegas Urban Growth Changes")
legend("topleft", legend = "Difference", fill = "red", cex = .7)

#color counties by amount of change:
#add precincts:
vegas_precincts <- vect("/Users/carterqberry/Downloads/Precincts/Precincts.shp")
#project to same coordinate system:
vegas_precincts <- project(vegas_precincts, "EPSG:4269")

#convert vegas_diff raster to polygons:
polygons <- terra::as.polygons(vegas_diff)

#crop to the same area:
vegas_precincts <- crop(vegas_precincts, vegas_diff)

# extract raster values for each precinct
values <- extract(vegas_diff, vegas_precincts)

# get the mean of the values for each precinct
mean_values <- sapply(values, function(x) mean(x, na.rm = TRUE))

# make a color palette
colors <- colorRampPalette(c("white", "red"))(100)

# normalize the mean values to range from 0 to 1
normalized_values <- (mean_values - min(mean_values, na.rm = TRUE)) / (max(mean_values, na.rm = TRUE) - min(mean_values, na.rm = TRUE))

# put the normalized values to colors
vegas_precincts$color <- colors[cut(normalized_values, breaks = seq(0, 1, length.out = 101), labels = FALSE)]

# plot (couldnt get it color as a gradient, but thats okay)
plot(vegas_precincts, col = vegas_precincts$color, main = "Las Vegas Precincts with Most Amount of Change")




# calculate the total number of pixels:
total_pixels_vegas <- ncell(landsat7_ndbi)
total_pixels_mead <- ncell(landsat7_mndwi)
total_pixels_vegas_9 <- ncell(landsat9_ndbi)
total_pixels_mead_9 <- ncell(landsat9_mndwi)

# calculate the number of urban pixels and water pixels:
urban_pixels_vegas <- sum(landsat7_ndbi[] > 0.05)
urban_pixels_vegas_9 <- sum(landsat9_ndbi[] > 0.05)
water_pixels_mead_9 <- sum(landsat9_mndwi[] > 0.1)

# check if there are any water pixels in the lake mead region:
if (sum(landsat7_mndwi[] > 0.2, na.rm = TRUE) > 0) {
  # calculate the number of water pixels
  water_pixels_mead <- sum(landsat7_mndwi[] > 0.2, na.rm = TRUE)
}

# get the percentage of urban area and water area
percentage_urban_vegas <- (urban_pixels_vegas / total_pixels_vegas) * 100
percentage_water_mead <- (water_pixels_mead / total_pixels_mead) * 100
percentage_urban_vegas_9 <- (urban_pixels_vegas_9 / total_pixels_vegas_9) * 100
percentage_water_mead_9 <- (water_pixels_mead_9 / total_pixels_mead_9) * 100

# round the percentages:
percentage_urban_vegas <- round(percentage_urban_vegas, 2)
percentage_water_mead <- round(percentage_water_mead, 2)
percentage_urban_vegas_9 <- round(percentage_urban_vegas_9, 2)
percentage_water_mead_9 <- round(percentage_water_mead_9, 2)

# print the results:
print(paste("Percentage of urban area in Las Vegas in 1999:", percentage_urban_vegas, "%"))
print(paste("Percentage of water area in Lake Mead in 1999:", percentage_water_mead, "%"))
print(paste("Percentage of urban area in Las Vegas in 2024:", percentage_urban_vegas_9, "%"))
print(paste("Percentage of water area in Lake Mead in 2024", percentage_water_mead_9, "%"))

print(paste("Urban area in Las Vegas grew by", percentage_urban_vegas_9-percentage_urban_vegas, "% from 1999-2024"))
print(paste("Water area in Lake Mead shrunk by", percentage_water_mead-percentage_water_mead_9, "% from 1999-2024"))


# calculate the differences:
urban_growth <- percentage_urban_vegas_9 - percentage_urban_vegas
water_decrease <- percentage_water_mead - percentage_water_mead_9

# create data for the pie charts:
labels_urban <- c("Urban Area in 1999", "Urban Growth in 2024)")
percentages_urban <- c(percentage_urban_vegas, urban_growth, 100 - percentage_urban_vegas_9)
labels_water <- c("Water Area in 2024", "Water Area Decrease from 1999)", "Other")
percentages_water <- c(percentage_water_mead, water_decrease, 100 - percentage_water_mead)

# create colors for the pie charts:
colors_urban <- c("blue", "lightblue", "white")
colors_water <- c("lightblue", "lightcoral", "white")

# create the pie charts:
pie(percentages_urban, labels = labels_urban, col = colors_urban, main = "Percentage of Urban Area")
pie(percentages_water, labels = labels_water, col = colors_water, main = "Percentage of Water Area")


