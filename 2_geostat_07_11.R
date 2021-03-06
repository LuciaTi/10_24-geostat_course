<<<<<<< HEAD
## Geostastics 07_11_2017
## Course Day 2

# set the working-directory
setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts/geostat_course_scripts")
# recall the working directory
getwd()

## 1) Introduction

# calculate time differences, here from System-Time until Christmas.
time_left_christmas <- difftime(("2017-12-24"), Sys.Date(), units = "days")
time_left_day <- difftime(("2017-12-02"), ("2017-12-01"), units = "hours")


## 2) Loading data-files  ####

# load the raster-package
library(raster) 

# read in a csv-file
data1 = read.table("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts/bio_data_forest.csv", 
                   header = TRUE, sep = ";")
# other usefull options: help-manual!!

# get some information about your data.
data1 # shows all the data
head(data1) # shows the first 6 rows of the data
summary(data1) # gives min, max, median and quartiles of the data
plot(data1)


## 3) creating a matrix ####
x <- matrix(c(4,7,3,8,9,2), nrow = 2) # matrix with two rows
x

x[2,2] # value row2, column 2
x[ , 2] # values all rows, column2

# create a bigger matrix
numbers_1 <- rnorm(80, mean = 0, sd = 1) # creates vector with n = 80 observations, these with mean = 0, sd = 1 and normal distributed.
mat_1 <- matrix(numbers_1, nrow =20, ncol = 4) # define the extents for the matrix and create it with values from numbers_1
mat_1

# transform to a data frame
df_1 = data.frame(mat_1)

names(df_1) <- c("var1", "var2", "var3", "var4") # add header / column names to the data frame
rownames(df_1) = c(1:20) # add rownames to the data frame, here numbers 1:20.

head(df_1) # show the beginning off the data frame.



## 4) queriing data points ####

df_1[, 1] # all rows, column 1
df_1$var1[1:3] # variable1, row 1:3
df_1[1:2, 1:2] # rows 1 and 2, column 1 and 2
df_1[df_1 <= 0] # all values <= 0 (actual values)
df_1[df_1 >= 0 & df_1 <= 1] # same with 2 conditions
df_1 <= 0 # all values that fit the condition (TRUE or FALSE)
(df_1 <= 0 | df_1 <= -1) # same with 2 conditions


# create a vector and query single values/groups of values.
x2 <- seq(1, 100, by =2.5)
x2
x2[1:10]
x2[20:30]
x2[length(x2)] # last value
x2[length(x2)-1] # second last value
x2[-1] # all but first position
position <- c(1,5,10) # create a vector with intersting positions
x2[position] # query the entries which fit to the condition of "position".
x2[-position] # query entries which don´t fit to the condition of "position".
x2 > 10
x2[x2 <= 10 | x2 >= 15] # gives actual data that fits to the condition
(x2 <= 10) | (x2 >= 15) # gives the positions that fit to the condition



x2[(x2 > 30)&(x2 < 70)] <- 2 # sets values >30 and <70 to 2

# faster possibility to change ceveral values at the same time
library(car)
x2 <- recode(x2, "0:20=1; 21:50=2; else=3") # changes severeal entries (following the condition, not the entry-number) at the same time.

#x2 <- numeric(length(x2)) # numeric: creates double precision vector of the given length, here with 40 entries.
#x2[x2 <= 30] <- 1 # set values smaller 30 to 1



# get information about the dataset.
summary(x2)
sum(x2)
cumsum(x2) # cumulativ sum from one entry to the next.

rev(x2) # reverts the order
sort(x2, decreasing = FALSE) # sorts the values
sample(x2, 10) # takes samples from x2, here 10 times 



## 5) Defining data frames ####
test <- data.frame(A = c(1,2,3), B=c("aB1", "aB2", "aB3")) # create a data frame with rows and columns.
test

test[, "A"] # queriing as before.
test$A # equal to command before.


# create data-frame with multiple variables and values.
df <- data.frame(plot = "location_name_1", measure1 = runif(100) * 1000, measure2 = round(runif(100) *100), 
                 value = rnorm(100, 2, 1), ID = rep(LETTERS, 100))
# runif creates random numbers (by default from [0,1]). Here n = 100 times.
df_2 <- data.frame(plot="location_name_2", measure1 = runif(50)*100, measure2 = round(runif(50)*10),
                  value = rnorm(50), ID = rep(LETTERS, 50))
df_3 = rbind(df, df_2) # connect the two data frames.


df_3[100:115, c("plot", "measure1", "measure2")] # rows 100:115, columns as specified.
df_3[df_3$value > 3.0, ]
df_3[df_3$value <=3 & df_3$value >= 2, ]

df_3$new_col <- df_3$measure1*df_3$measure2 # add a new column to the data frame (here multiplication from existing columns)






=======
## Geostastics 07_11_2017
## Course Day 2

# set the working-directory
setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts/geostat_course_scripts")
# recall the working directory
getwd()

## 1) Introduction

# calculate time differences, here from System-Time until Christmas.
time_left_christmas <- difftime(("2017-12-24"), Sys.Date(), units = "days")
time_left_day <- difftime(("2017-12-02"), ("2017-12-01"), units = "hours")


## 2) Loading data-files  ####

# load the raster-package
library(raster) 

# read in a csv-file
data1 = read.table("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts/bio_data_forest.csv", 
                   header = TRUE, sep = ";")
# other usefull options: help-manual!!

# get some information about your data.
data1 # shows all the data
head(data1) # shows the first 6 rows of the data
summary(data1) # gives min, max, median and quartiles of the data
plot(data1)


## 3) creating a matrix ####
x <- matrix(c(4,7,3,8,9,2), nrow = 2) # matrix with two rows
x

x[2,2] # value row2, column 2
x[ , 2] # values all rows, column2

# create a bigger matrix
numbers_1 <- rnorm(80, mean = 0, sd = 1) # creates vector with n = 80 observations, these with mean = 0, sd = 1 and normal distributed.
mat_1 <- matrix(numbers_1, nrow =20, ncol = 4) # define the extents for the matrix and create it with values from numbers_1
mat_1

# transform to a data frame
df_1 = data.frame(mat_1)

names(df_1) <- c("var1", "var2", "var3", "var4") # add header / column names to the data frame
rownames(df_1) = c(1:20) # add rownames to the data frame, here numbers 1:20.

head(df_1) # show the beginning off the data frame.



## 4) queriing data points ####

df_1[, 1] # all rows, column 1
df_1$var1[1:3] # variable1, row 1:3
df_1[1:2, 1:2] # rows 1 and 2, column 1 and 2
df_1[df_1 <= 0] # all values <= 0 (actual values)
df_1[df_1 >= 0 & df_1 <= 1] # same with 2 conditions
df_1 <= 0 # all values that fit the condition (TRUE or FALSE)
(df_1 <= 0 | df_1 <= -1) # same with 2 conditions


# create a vector and query single values/groups of values.
x2 <- seq(1, 100, by =2.5)
x2
x2[1:10]
x2[20:30]
x2[length(x2)] # last value
x2[length(x2)-1] # second last value
x2[-1] # all but first position
position <- c(1,5,10) # create a vector with intersting positions
x2[position] # query the entries which fit to the condition of "position".
x2[-position] # query entries which don´t fit to the condition of "position".
x2 > 10
x2[x2 <= 10 | x2 >= 15] # gives actual data that fits to the condition
(x2 <= 10) | (x2 >= 15) # gives the positions that fit to the condition



x2[(x2 > 30)&(x2 < 70)] <- 2 # sets values >30 and <70 to 2

# faster possibility to change ceveral values at the same time
library(car)
x2 <- recode(x2, "0:20=1; 21:50=2; else=3") # changes severeal entries (following the condition, not the entry-number) at the same time.

#x2 <- numeric(length(x2)) # numeric: creates double precision vector of the given length, here with 40 entries.
#x2[x2 <= 30] <- 1 # set values smaller 30 to 1



# get information about the dataset.
summary(x2)
sum(x2)
cumsum(x2) # cumulativ sum from one entry to the next.

rev(x2) # reverts the order
sort(x2, decreasing = FALSE) # sorts the values
sample(x2, 10) # takes samples from x2, here 10 times 



## 5) Defining data frames ####
test <- data.frame(A = c(1,2,3), B=c("aB1", "aB2", "aB3")) # create a data frame with rows and columns.
test

test[, "A"] # queriing as before.
test$A # equal to command before.


# create data-frame with multiple variables and values.
df <- data.frame(plot = "location_name_1", measure1 = runif(100) * 1000, measure2 = round(runif(100) *100), 
                 value = rnorm(100, 2, 1), ID = rep(LETTERS, 100))
# runif creates random numbers (by default from [0,1]). Here n = 100 times.
df_2 <- data.frame(plot="location_name_2", measure1 = runif(50)*100, measure2 = round(runif(50)*10),
                  value = rnorm(50), ID = rep(LETTERS, 50))
df_3 = rbind(df, df_2) # connect the two data frames.


df_3[100:115, c("plot", "measure1", "measure2")] # rows 100:115, columns as specified.
df_3[df_3$value > 3.0, ]
df_3[df_3$value <=3 & df_3$value >= 2, ]

df_3$new_col <- df_3$measure1*df_3$measure2 # add a new column to the data frame (here multiplication from existing columns)






>>>>>>> bdb9e9412848493df534c1439d5e296597869345
