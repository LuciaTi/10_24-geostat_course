# Course Script 14_11_17

setwd("C:/Users/s331737/Desktop/Lucia Tischer")

## 1) repetion: reading in data ####
test_data <- read.table("test_data.csv", header = TRUE, sep = ";")
summary(test_data)

plot(test_data$variable_1 ~ test_data$plot, 
     ylim = c(0, 100))
lines(lowess(test_data$variable_1, f = .2), col = "red")
points(test_data$variable_2 ~ test_data$plot)
lines(lowess(test_data$variable_2, f = .2), col = "blue")

## 2) exploring the function Cut() ####
c <-  rnorm(100, mean = 0, sd = 1) # create a vector with 100 random numbers
c_cut <- table(cut(c, breaks = -2:1)) # cut the vector in groups, breaks are cutting points, intervalls closed on the rigth.
# also see homework,  07_11

## 3) repetition: extracting/query values, indexing vectors
x <- seq(1, 100, by = 1) # create a vector from 1 to 100.

x[length(x)-1] # extract only second last value
x[-10] # extract all but 10th value.
x[-(length(x)-1)] # extract all but second last value
query <- c(1, 10, 20, 50, 100) # create a mask vor qeriing
x[query] # extract all the values defined by the mask.
x > 20 # extract, if values are over 20.
x[x > 20] # extract THE REAL values, if they are over 20.
x >20 & x < 30 # exract, IF values are over 20 AND under 30
x > 20 | x < 15 # extract, IF values are over 20 OR under 15
x[x > 20 | x < 15] # extract THE REAL values at the position, where values over 20 OR under 15.

x2 <- numeric(length(x)) # create a vector with the length of x (values will be cero.)

library(car) # fast recoding of vector-values with the function car::recode().
x3 <- recode(x, "0:20=1;20:30=2;else=3") # recode the values, "all values [0, 20] shell be 1", etc.
                                         # --> if condition doubles (here two times twenty): first argument passes.
x3[30]
rev(x) # reverses the order of the vector
sort(x) # sorts the vector-values in ascending order
sort(x, decreasing = TRUE) # same in descending order
sample(x, 10) # ten random samples are picked from x.

## 3) repetition: create a matrices and extracting values ####
# by row ...
m1 <- matrix(c(2,4,3,1,5,7), 
             nrow = 2, 
             ncol = 3, 
             byrow = TRUE)
m1

# ... and by column
m2 <- matrix(c(2,4,3,1,5,7), 
             nrow = 2, 
             ncol = 3, 
             byrow = FALSE)
m2 # extract all lines and columns.

m2[, 2] # extract 2nd colum.
m2[c(1,2), c(1,3)] # etract line 1 and 2 from colum 1 and 3.

m2>3 # extract position in m2 of values > 3.
m2[m2>3] # extract the REAL VALUES in m2 which are > 3.


numbers_1 <- rnorm(80, mean = 0, sd = 1) # create a vector, 80 values, normal distribution data.
mat_1 <- matrix(numbers_1, nrow = 20, ncol = 4) # safe values in matrix
df_1 <- data.frame(mat_1) # transfer matrix to class data.frame
names(df_1) <- c("var1", "var2", "var3", "var4") # add the column-names to the data-frame.

test <- data.frame(A = c(1,2,3), B=c( "aB1" , "aB2" , "aB3")) # create data.frame with 2 columns
test[, 2] # extract all lines from column 2
test$A # equivalent call.
test[ ,"A"] # equivalent call.
test # show all the matrix




# create a data-frame with multiple variables and values.
df <- data.frame(plot = "location_name_1", measure1 = runif(100) * 1000, measure2 = round(runif(100) *100), 
                 value = rnorm(100, 2, 1), ID = rep(LETTERS, 100))
# runif creates random numbers (by default from [0,1]). Here n = 100 times.
df_1 <- data.frame(plot="location_name_2", measure1 = runif(50)*100, measure2 = round(runif(50)*10),
                   value = rnorm(50), ID = rep(LETTERS, 50))
df_2 <- data.frame(plot="location_name_3", measure1 = runif(50)*500, measure2 = round(runif(50)*50),
                   value = rnorm(50), ID = rep(LETTERS, 50))
df_3 = rbind(df, df_1, df_2) # connect the three data frames.


# do some data-checks.
summary(df_3) # show stastic summary.
length(df$measure1) # query the length/number of values.
length(df_3$measure1)
str(df_3) # show information about data
head(df_3) # show first few lines.
mode(df_3) # show saving-mode of df_3

head(df[ , c("plot", "measure1", "measure2")]) # show all lines from only these columns of df.

head(df[grep("a", df$ID, ignore.case = TRUE), ]) # query data by using a keyword.
# ignore.case = TRUE: all similar values are counted, also if difference small/capital letter exists + also partial strings are matched
# ignore.case = FALSE: only taken into account, if strings match EXACTLY the keyword.


# save only these 4 columns as new data.frame and plot the data.
x <- df_3[ , c("plot", "measure1", "measure2", "ID")]

ggplot(x)+                                         # second possibility
  geom_point(aes(x=x$measure1,y=x$measure2,colour=x$ID))+
  ggtitle("Measurements on different plots") +
  xlab("measure 1") + 
  ylab("measure 2") +
  theme(axis.title.x = element_text(face = "italic", colour = "black", size = 14), 
        axis.title.y = element_text(face = "italic", colour = "black", size = 14),
        plot.title = element_text(face="bold", color="red", size=16), 
        legend.title = element_text(face = "plain", colour = "black", size = 14),
        legend.position="bottom",
        legend.text=element_text(size=11)) +
  labs(colour= "Plot-ID")



# plot the whole data for plot and measure 1 and 2.
x3 <- df_3[ , c("plot", "measure1", "measure2")] # define the data.

library(ggplot2)
qplot(measure1, measure2, colour = plot, data = x3) # first possibility.

ggplot(x3)+                                         # second possibility
  geom_point(aes(x=x3$measure1,y=x3$measure2,colour=x3$plot))+
  ggtitle("Measurements on different plots") +
  xlab("measure 1") + 
  ylab("measure 2") +
  theme(axis.title.x = element_text(face = "italic", colour = "black", size = 14), 
        axis.title.y = element_text(face = "italic", colour = "black", size = 14),
        plot.title = element_text(face="bold", color="red", size=16), 
        legend.title = element_text(face = "plain", colour = "black", size = 14),
        legend.position="bottom",
        legend.text=element_text(size=11)) +
  labs(colour= "Plots")


plot(x3$measure1, x3$measure2, col = x3$plot)       # third possibility.
legend("bottomright", legend=levels(x3$plot), pch = 15, col = unique(x3$plot))


# plot only the data from lines 66:70, 3000:3010 and 5000:5010,  for plot, measure1 and measure2.
x4 <- df_3[c(66:70, 3000:3010, 5000:5010), c("plot", "measure1", "measure2")]
ggplot(x4)+                                         # second possibility
  geom_point(aes(x=x4$measure1,y=x4$measure2,colour=x4$plot))+
  ggtitle("Measurements on different plots") +
  xlab("measure 1") + 
  ylab("measure 2") +
  theme(axis.title.x = element_text(face = "italic", colour = "black", size = 14), 
        axis.title.y = element_text(face = "italic", colour = "black", size = 14),
        plot.title = element_text(face="bold", color="red", size=16), 
        legend.title = element_text(face = "plain", colour = "black", size = 14),
        legend.position="bottom",
        legend.text=element_text(size=11)) +
  labs(colour= "Plots")



# explore ggplot-boxplots.
# therefore: first create new data.
df <- data.frame(plot = "location_name_1", measure1 = runif(100) * 500, measure2 = round(runif(100) *100), 
                 value = rnorm(100, 2, 1), ID = rep(LETTERS, 100))
# runif creates random numbers (by default from [0,1]). Here n = 100 times.
df_1 <- data.frame(plot="location_name_2", measure1 = runif(50)*500, measure2 = round(runif(50)*10),
                   value = rnorm(50), ID = rep(LETTERS, 50))
df_2 <- data.frame(plot="location_name_3", measure1 = runif(50)*500, measure2 = round(runif(50)*50),
                   value = rnorm(50), ID = rep(LETTERS, 50))
df_3 <- data.frame(plot="location_name_4", measure1 = runif(50)*500, measure2 = round(runif(50)*100),
                   value = rnorm(50), ID = rep(LETTERS, 50))
df_4  = rbind(df, df_1, df_2, df_3) # connect the three data frames.

x <- df_4[ , c("plot", "ID", "measure1", "measure2", "value")]


ggplot(data = x, aes(x=x$plot,y=x$value, col = x$plot)) + 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=2) +
  ggtitle("Measurements on different plots\nBoxplots") +
  xlab("Plotname") + 
  ylab("value") +
  theme(plot.title=element_text(face="bold", color="red", size=16), 
        axis.title.x =element_text(face= "italic", color="black", size=14),
        axis.title.y =element_text(face= "italic", color="black", size=14),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(colour= "Plots")



# explore the coplot() fumction:
coplot(x$measure2 ~ x$measure1 | x$plot, 
       col = x$plot, 
       xlab = c("measure1", "plot"), 
       ylab = "measure2")




## 4) Start with raster data ####

library(raster)
r1 <- raster(nrows =10, ncols =10) # create a raser with 10 rows and 10 columns
r1 # get information about the raster
plot(r1) # plot the empty raster --> no values associated.
r1[] <- rnorm(100) # fill the empty raster with 100 random values.
plot(r1) # and plot these values.

# create a vector (spatial points)
library(sp)
poi1 <- cbind(c(rnorm(10)), c(rnorm(10))) # create 10x10 random coordinates
poi1 # show the coordinates
poi1.sp <- SpatialPoints(poi1) # convert the list of coordinates to a spatial object
plot(poi1.sp) # plot this object.

# create a SpatialPointsDataFrame
df <- data.frame(attr1 = c("a", "b", "z", "d", "e", "q", "w", "r", "z", "y"), attr2 =c(101:110))
poi1.spdf <- SpatialPointsDataFrame(poi1.sp, df)
plot(poi1.spdf)



## example data set: lsat (package RStoolbox)
library(RStoolbox)
lsat # information about the data set.
data(lsat) # load/crate the example data from the package.
x <- lsat[1:10, ] # values of rows 1:10
x <- lsat[] # all values
x <- getValues(lsat) # all values
x <- lsat[lsat$B2_dn < 20] # only values, where value from band 2 is < 20.

# plot only band 1
plot(lsat$B1_dn)
plot(lsat[[1]])

# extraxt band 2 and 3 and save them in a new object.
lsat_extract <- lsat[[2:3]]
lsat_extract <- lsat[[c(2,3)]]
lsat_extract <- c(lsat$B2_dn, lsat$B3_dn)

# query or mask values from lsat.
lsat_sub <- lsat[8, 10] # pixel of 8th row and 10th column per band
lsat_sub <- lsat[1:10, ] # pixels from rows 1:10 in each column per band
lsat_sub <- lsat[500] # value per band with the cell-ID 500
queryRaster <- (lsat[[(2)]] < 20) # create a query raster: in band 2 show only values which are < 20
lsat_sub <- lsat[queryRaster]
head(lsat_sub)

# get the corner pixels of the raster data
nr <- nrow(lsat)
nc <- ncol(lsat)
lsat[c(1, nr), c(1, nc)] # (for each band)
lsat$B4_dn[c(1, nr), c(1, nc)] # (for band 4)

# some information about the data.
length(lsat[[2]]) # number of values in band 2
cellStats(lsat, max) # maximum value of each band
cellStats(lsat$B2_dn, min) # minimum value of band 2
lsat[] <- rnorm(ncell(lsat)) # cells are filled with normal distributed data. ncell is number of entries.
lsat[lsat < 0] <- NA # set all values in lsat <0 to NA



# load example-vector data from the RStoolbox package:
poly <- readRDS(system.file("external/trainingPolygons.rds", package="RStoolbox"))
poly # --> Spdf with 36 features

# create a new raster layer with proberties of poly and 100 values
env <- raster(poly, vals = rnorm(100))  # poly defines the properties for the new raster Layer (same as for object poly)
                                        # vals: values for the new raster layer.

# extract values from env at the location of another spdf, here poly
extract(env, poly)

env[] <- 0 # set all values in env to cero
env[poly] <- 1 # set areas of poly to 1

