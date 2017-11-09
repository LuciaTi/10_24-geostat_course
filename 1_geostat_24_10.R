# 24.10.2017
# first meeting "Geostatistics MB-02"
rm(list = ls())

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts")


## 1) First Explanations ####
# activating package:  library()
# read.table() for importing data frames/ spreadsheets.
# write.table for exporting data frames

# set working directory with setwd()
# ?command for information, for example:
  ?read.table()
  # or press F1 --> opens help page as well (usefull: Help > Keyboard Shortcuts).
# ??command for broader information


#R as a calculator.
5+6
5 * 5^4 - 88 * 15

# define variables and recall them.
result1 <- 5 + 6
result1

# seq() creates sequences and plot it, f.e.
seq(50, 100, 5) # sequence from 50 to 100 each 5th step
plot(seq(50, 100, 5))

# q() quit R. q("n") for quitting without saving.

# create vectors and recall them, f.e.
x <- c("A", 1:10)
x



# enter the monthly precipitation values of Germany (no real values!!!)
prec_avg1 <- c(56, 46, 50, 53, 69, 83, 83, 80, 62, 55, 60, 63)
plot(prec_avg1, 
     pch = 19, 
     col = "red", 
     cex = 1.5, 
     ylab = "average precipitation", 
     xlab = "month", 
     cex.lab = 1.5)
lines(lowess(prec_avg1, f = .2)) # add a line connecting the points.




## 2) start with raster data - precipitation ####
## 2 A) precipitation of germany #### 
library(raster)

# ISO3-Codes: getData("ISO3")

# get the surface/shape/map of country borders of germany
germany <- getData("GADM", country = "DEU", level = 2)
plot(germany)

# get the precipitation data of germany
prec_germany <- getData("worldclim", var = "prec", res = .5, lon = 10, lat = 51)
plot(prec_germany) 

# change the resolution (just for fun/ for trying)
prec_germany_test <- getData("worldclim", var = "prec", res = 2.5, lon = 10, lat = 51)
plot(prec_germany_test) # --> larger extent is downloaded

# crop the precipitation data to the extend of germany`s country borders
prec_germany1 <- crop(prec_germany, germany) # crop "reduces" image to extent of germany.
spplot(prec_germany1)

prec_germany2 <- mask(prec_germany1, germany) # mask "reduces" image to surface of germany.
spplot(prec_germany2)

# change mask and crop commands for trying/ for fun. Try the inverse option in the mask-function.
prec_germany_test1 <- mask(prec_germany, germany)
plot(prec_germany_test1)
prec_germany_test2 <- crop(prec_germany, germany)
plot(prec_germany_test2) # if order is changed, the whole extent within germany is placed, is shown (not limited at the borders)

# calculate and plot the mean precipitation values:
prec__germany_avg <- cellStats(prec_germany2, stat = "mean")
plot(prec_avg, 
     pch = 19, 
     col = "red", 
     cex = 1.5, 
     ylab = "average precipitation", 
     xlab = "month", 
     cex.lab = 1.5)
lines(lowess(prec__germany_avg, f = .2)) # add a line connecting the points. f: smoothness-factor, larger values give more smoothness

## 2 B) precipitation of bavaria #### 
bavaria = germany[germany$NAME_1 == "Bayern", ] # define the part of the germany-data for bavaria
plot(bavaria)
prec_bavaria = mask(crop(prec_germany, bavaria), bavaria)
plot(prec_bavaria)

# plot the precipitation of Bavaria in December compared to July
par(mfrow = c(1,2), mar = c(4,4,5,5), mgp = c(2, 1, 0))
plot(prec_bavaria$prec12_16, 
     xlab = "December", 
     legend.args=list(text="precipitation [ml]", side = 2, font=2, line = 0.5, cex=1.1))
plot(prec_bavaria$prec7_16, 
     xlab = "July", 
     legend.args=list(text="precipitation [ml]", side = 2, font=2, line = 0.5, cex=1.1))
# mtext("Precipitation Bavaria\nDecember vs. July", side = 3, line = 3, outer = T)  --> metext() didn?t work ...?

## 2 C) precipitation of germany vs. south Africa #### 
#plot the precipitation of South Africa (compared to Germany - boxplot)
library(raster) # load the raster package
getData("ISO3") # get the ISO-code from South Africa --> ZAF
s_africa = getData("GADM", country = "ZAF", level = 2) # get the country boarders
plot(s_africa)
# get long(x) and lat(y) values
mean(c(xmin(s_africa), xmax(s_africa))) # long value: 24
mean(c(ymin(s_africa), ymax(s_africa))) # lat value: - 29
prec_s_africa.1 = getData("worldclim", var = "prec", res = .5, lon = 24, lat = -29) # get the precipitation data --> not all is covered
prec_s_africa.2 = getData("worldclim", var = "prec", res = .5, lon = 23, lat = -33)
prec_s_africa.3 = getData("worldclim", var = "prec", res = .5, lon = 31, lat = -28)
prec_s_africa = mosaic(prec_s_africa.1, prec_s_africa.2, prec_s_africa.3, fun = mean) # connect the datasets
prec_s_africa2 = mask(crop(prec_s_africa, s_africa), s_africa) # reduce the data to the country borders of south Africa.
plot(prec_s_africa2) # check of data is correct

prec_s_africa_avg <- cellStats(prec_s_africa2, stat = "mean") # calculate mean vlaues for south africa

prec__germany_avg <- cellStats(prec_germany2, stat = "mean")


par(mgp = c(3, 1.5, 0), mfrow = c(1,1)) # move labels further from the plot
prec_total_avg = cbind(prec__germany_avg, prec_s_africa_avg) # connect the precipitation from germany and s_africa to one dataframe
# and plot it comparing the countries in a boxplot
boxplot(prec_total_avg, 
        xlab = "Countries",
        ylab = "Mean Precipitation [ml]", 
        names = c("mean\nGermany", "mean\nSouth Africa"), 
        main = "Mean Precipitation\nGermany vs. South Africa", 
        cex.lab = 1.3, 
        cex.names = 1.3, 
        cex.axis = 1.2, 
        las = 1)


## 3) Coding to Report ####

# using the packages knitr or slidifiy to code/run analysis and present at the same time.
install.packages(rmarkdown)

# see file: presentation 24_10


## 4) Introduction to ggplot2-Plotting ####

## Data from Steigerwald available on bitbucket.org --> download for these examples
install.packages("devtools")
library(devtools)
install_bitbucket("EAGLE_MSc/Steigerwald", build_vignettes = TRUE)

library(st.data)
data("bio_data")
bio_data

st.data



## Start with ggplot2

library(ggplot2)

ggplot(mpg, aes(x = year)) + # mpg is the dataset, x = the selected colomn for plotting (?)
  geom_histogram()

ggplot(mpg, aes(x = cyl)) + 
  geom_histogram()


# x11() # opens a different plotting window
# define the dataset
x <- data.frame(x = 1, y = 1, label = "ggplot2 introduction \n @ EAGLE") 
# plot the dataset.
# Therefore, the previously defined x and y values are used. the defined label is plottet at this place.
ggplot(data = x, aes(x = x, y = y)) + geom_text(aes(label = label), size = 15) 


# creat a simple dot plot
head(mpg)
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

# add information around this plot
ggplot(mpg, aes(displ, cty, colour = class)) +
  geom_point() +
  geom_smooth()

# divide data into groups
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class) +  # devides the data into classes/groups for plotting
  geom_smooth() # adds a smoothed regression line



# connect several geom attributes
ggplot(mpg, aes(class, displ)) +
  geom_boxplot(alpha = .5) +
  geom_point(aes(color = hwy), alpha = 0.7, size = 1.5, position = position_jitter(width = 0.25, height = 0))
# !!! Width kann unter Umst?nden die Daten verf?lschen (zieht einzelen Messpunkte falls ?bereinander auseinander)



# store the plot - see Rest after "..." on presentation
myPlot1 <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

# call the plot
myPlot

# call the plot and add another geom attribute
myPlot + geom_smooth()



## see rest of presentation and examples!!