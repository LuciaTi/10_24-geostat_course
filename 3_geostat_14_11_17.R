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
test




# create a data-frame with multiple variables and values.
df <- data.frame(plot = "location_name_1", measure1 = runif(100) * 1000, measure2 = round(runif(100) *100), 
                 value = rnorm(100, 2, 1), ID = rep(LETTERS, 100))
# runif creates random numbers (by default from [0,1]). Here n = 100 times.
df_2 <- data.frame(plot="location_name_2", measure1 = runif(50)*100, measure2 = round(runif(50)*10),
                   value = rnorm(50), ID = rep(LETTERS, 50))
df_3 = rbind(df, df_2) # connect the two data frames.
summary(df_3)
length(df$measure1)
length(df_3$measure1)

df[ , c("plot", "measure1", "measure2")]

# save only these 4 columns as new data.frame
x <- df[ , c("plot", "measure1", "measure2", "ID")]
x2 <- df[ , c("measure1", "measure2")]

par(mfrow = c(1,1))
plot(x$measure1[x$ID == "A" ] ~ x$measure2[x$ID == "A" ], 
     col = "red")
points(x$measure1[x$ID == "B" ] ~ x$measure2[x$ID == "B" ], 
     col = "blue")

# try plotting with different coments...
plot(x$measure2 ~ x$ID)
plot(x)
plot(x$plot, x$measure1, 
     type = "p")


## 4) Start with raster data ####

library(raster)
r1 <- raster(nrows =10, ncols =10)
r1[] <- rnorm(100)
plot(r1)

# list of values is transvered to a spatial object and plotted:
library(sp)
poi1 <- cbind(c(rnorm(10)), c(rnorm(10)))
poi1
poi1.sp <- SpatialPoints(poi1)
plot(poi1.sp)

df <- data.frame(attr1 = c("a", "b", "z", "d", "e", "q", "w", "r", "z", "y"), attr2 =c(101:110))
poi1.spdf <- SpatialPointsDataFrame(poi1.sp, df)
plot(poi1.spdf)


library(RStoolbox)
lsat

# plot band 2
plot(lsat[[2]])
plot(lsat$B2_dn)

lsat_extract <- lsat[[2:3]]
lsat_extract <- lsat[[c(2,3)]]
lsat_extract <- c(lsat$B2_dn, lsat$B3_dn)

lsat_2_100 <- lsat[lsat[[2]] <= 100] # values of band 2 <= 100
 
x = getValues(lsat) 

x <- lsat[]
x <- lsat[lsat == 10]

length(lsat[[2]])
cellStats(lsat, max) # maximum value of each band
cellStats(lsat$B2_dn, min) # minimum value of band 2



library(move)
data("leroy")
env <- raster(leroy, vals =rnorm(100)) # leroy is added to environment
plot(leroy)
plot(lsat)
