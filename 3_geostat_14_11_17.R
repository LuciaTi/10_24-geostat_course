# Course Script 14_11_17

setwd("C:/Users/s331737/Desktop/Lucia Tischer")

## repetion ####
# example data is read in.
test_data <- read.table("test_data.csv", header = TRUE, sep = ";")
summary(test_data)

plot(test_data$variable_1 ~ test_data$plot, 
     ylim = c(0, 100))
lines(lowess(test_data$variable_1, f = .2), col = "red")
points(test_data$variable_2 ~ test_data$plot)
lines(lowess(test_data$variable_2, f = .2), col = "blue")

# create a vector
c <-  rnorm(100)
# explore the comand cut()
# ...

# how to extract/query values ...

library(car)
x <- seq(1, 100, by = 2.5)
x2 <- numeric(length(x))
x2 <- recode(x, "0:30=1;30:70=2;else=3")
x2[30] # should be 1 !!
rev(x) # reverses the order

# create a matrix by row
m2 <- matrix(c(2,4,3,1,5,7), 
             nrow = 2, 
             ncol = 3, 
             byrow = TRUE)
m2

# ... and by column
m3 <- matrix(c(2,4,3,1,5,7), 
             nrow = 2, 
             ncol = 3, 
             byrow = FALSE)
m3


numbers_1 <- rnorm(80, mean = 0, sd = 1)
mat_1 <- matrix(numbers_1, nrow = 20, ncol = 4)
df_1 <- data.frame(mat_1)
names(df_1) <- c("var1", "var2", "var3", "var4")
test <- data.frame(A = c(1,2,3), B=c( "aB1" , "aB2" , "aB3"))
test[, 2]
test

## repitition:
# create data-frame with multiple variables and values.
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


## Start with raster data ####

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
