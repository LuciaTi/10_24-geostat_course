# Course Script day 4, 21.11.17

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts/geostat_course_scripts")


# 1) Repetition -creating raster and vector data ####
# task: create data and recode it.
library(car)
random_data <- rnorm(n = 10, mean = 20, sd = 5)
random_data_2 <- recode(random_data, "1:5=1;6:20;else=3")

# create raster data
library(raster)
r1 <- raster(nrows =10, ncols =10)  # empty raster 
r1[] <- rnorm(100)                  # fill raster with values
r2 <- raster(nrows =10, ncols =10)  # same for second raster
r2[] <- rnorm(100)
raster1 <- stack(r1, r2)            # stack the two rasters together
raster1[[3]] <- rnorm(100)          # add third raster directly
plot(r1)                            



# create a vector data (spatial points)
library(sp)
poi1 <- cbind(c(rnorm(10)), c(rnorm(10))) # create 10x10 random coordinates
poi1 # show the coordinates
poi1.sp <- SpatialPoints(poi1) # convert the list of coordinates to a SpatialPointsObject
plot(poi1.sp) # plot this object.

df <- data.frame(attr1 =c("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), # create a dataframe with two attributes.
                 attr2 =c(1:10))
poi1.spdf <- SpatialPointsDataFrame(poi1.sp, df)
plot(poi1.spdf)





# load example raster data.
library(RStoolbox)
data(lsat)
lsat
plot(lsat$B1_dn)


## 2) If, while statements ####

a <-  sqrt(2) 
if(a*a != 2) # define the condition.
{
  print("R is great!")
}


j <- 0
while(j < 1)
{
  j <- j+0.1 ; print(j)
}


## 3) Defintion of functions ####

myfunction <- function(x, y){
  z <- x+y
  return(c("z is:", z))
}
myfunction(4,3)

# or

myfunction <- function(x, y){   # --> same function. return only needed, if you create many values in the function which are needed later on.
  x+y
}
myfunction(4,3)


# functions for remote sensing
fun_ndvi <- function(nir, red){(nir-red)/(nir+red)}


## 3) Working with raster data ####
library(raster)
library(RStoolbox) 
data(lsat) # load raster example data
writeRaster(lsat,
            "C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts/geostat_course_scripts/lsat.grd")
writeRaster(lsat,
            "C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts/geostat_course_scripts/lsat.tif")


# loading raster data
band_1 <-  raster("crop_p224r63_all_bands.tif", band=1) # load raster from working directory
band_2 <-  raster("crop_p224r63_all_bands.tif", band=2)
all_bands <- stack(band_1, band_2)
all_bands1 <- brick("crop_p224r63_all_bands.tif")

# adding or deleting bands
allPlus <- stack(all_bands1, all_bands$crop_p224r63_all_bands.1) # stack another additional band to the raster
allPlus_minus <- dropLayer(allPlus, 6) # delete the same layer again (it´s the sixth layer)

#query some information about the raster
cellStats(all_bands1$crop_p224r63_all_bands.1, max)
summary(all_bands1)


all_bands2 <- brick("lsat.tif") # load raster data from harddisk


# plotting raster data
library(ggplot2)
plotRGB(all_bands1, 3,2,1, stretch="lin")

ggR(all_bands1, layer = 1, stretch = "lin", geom_raster = TRUE) +
  scale_fill_gradient(low = "blue", high = "green")

ggRGB(all_bands1, 3,2,1, stretch = "lin")

ggR(all_bands1, layer = 4, maxpixels = 1e6, stretch = "hist")


# export raster data
writeRaster(all_bands1, dataType = "FLT4S", filename = "allbands_1.tif", format = "GTiff", overwrite = TRUE)
KML(all_bands2, allbands1, col = rainbow(100), maxpixels = 100000)


# cropping data
plot(all_bands1$crop_p224r63_all_bands.5)
ext <- drawExtent()
band_5_crop <- crop(all_bands1$crop_p224r63_all_bands.5, ext)
plot(band_5_crop)

ext * 2


# band calculation/ ... with functions
raster_sd <- calc(all_bands1, fun = sd) # calc works for all bands, not for single  rasters!
plot(raster_sd)

fun <- function(x){x/10}
raster_output <-  calc(band_1, fun)
plot(raster_output)

fun2 <- function(x){x[is.na(x)] <- -999; return(x)}
raster_output2 <- calc(band_1, fun2)
plot(raster_output2)

raster_output3 <- calc(all_bands1, fun = function(x){x[1]+x[2]*x[3]})
plot(raster_output3)


# define extent interactively
plot(all_bands1$crop_p224r63_all_bands.4)
ext <- drawExtent()
all_bands1_crop <- crop(all_bands1, ext)



# Check, how pixel values from sepparate layers are linked.
# Here: compute a linear model, bands[1:5] are depending variable, bands[6:10] are explaining.
# band1/band6, band2/band7 and so on.
# coefficients: 1 is intercept, 2 is slope
raster12 <- stack(all_bands1_crop, all_bands1_crop)
fun <- function(x){lm(x[1:5] ~ x[6:10])$coefficients[2]}
raster_output4 <- calc(raster12, fun)
plot(raster_output4)
calc(raster12, fun = fun, filename ="raster12_lm") # write calculated file directly to harddisk.





## 4) vegetation indeces in R ####

# three different possibilities to calculate the NDVI
lsat_ndvi <- (lsat$B4_dn - lsat$B3_dn)/(lsat$B4_dn + lsat$B3_dn)
plot(lsat_ndvi)

# overlay normally for single-layer objects (RasterLayer)
lsat_ndvi2 <- overlay(lsat$B4_dn, lsat$B3_dn, fun = function(nir, red){(nir-red)/(nir+red)})
plot(lsat_ndvi2)

# calc normally for mulitylayer-objects, like RasterStack or RasterBrick
lsat_ndvi3 <- calc(lsat, fun = function(x){(x[, 4] - x[, 3])/(x[, 4] + x[, 3])})
plot(lsat_ndvi3)


# other vegetation indices:
lsat_MSAVI <- overlay(lsat$B4_dn, lsat$B3_dn, 
                      fun = function(nir, red){(2*nir+1-sqrt((2*nir + 1)^(2*nir + 1) - 8*(nir-red)))})
plot(lsat_MSAVI)

rvi <- function(nir, red){nir/red}
lsat_rvi <- overlay(lsat$B4_dn, lsat$B3_dn, fun = rvi)
plot(lsat_rvi)


lsat_ndvi <- spectralIndices(lsat, red = "B3_dn", nir = "B4_dn", indices = "NDVI")
plot(lsat_ndvi)
lsat_DVI <- spectralIndices(lsat, red = "B3_dn", nir = "B4_dn", indices = "DVI")
lsat_slavi <- spectralIndices(lsat, red = "B3_dn", nir = "B4_dn", indices = "SAVI")
plot(lsat_slavi)

vi_stack <- stack(lsat_DVI, lsat_ndvi, lsat_MSAVI)
lsat_VI_sd <- calc(vi_stack, fun = sd)
plot(lsat_VI_sd)
