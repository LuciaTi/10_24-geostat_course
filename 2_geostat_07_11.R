## Geostastics 07_11_2017
## Course Day 2

setwd("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/geostat_course_scripts")


## 1) Introduction

# calculate time differences
difftime(("2017-12-24"), Sys.Date(), units = "days")
# getData( falls var =" auf 5 oder 10 gesetzt: ganzer Globus wird geladen --> kein lat long notwendig)


## 2) Vector, Data Frames and indexing ####

library(raster) # load the raster-package

# read in data
data1 = read.table("C:/Users/Ltischer/Documents/Studium/A Master/Geostatistics/R-Skripte/11_07_geostat_course/bio_data_forest.csv", 
                   header = TRUE, sep = ";")
data2 = read.csv()

# get some information about your data.
data
head()
summary()
plot()




## create a matrix.
x <- matrix(c(4,7,3,8,9,2), nrow = 2) # matrix with two rows
x

x[2,2]
x[ , 2]

# create a bigger matrix
numbers_1 <- rnorm(80, mean = 0, sd = 1)
mat_1 <- matrix(numbers_1, nrow =20, ncol = 4)
mat_1

# transform to a data frame
df_1 = data.frame(mat_1)

# add header / column names to the data frame
names(df_1) <- c("var1", "var2", "var3", "var4")

head(df_1) # show the beginning off the data frame.

df_1[, 1] # query data points.
df_1$var1[1:3]
df_1[1:2, 1:2]


# create a vector and query single values/groups of values.
x2 <- seq(1, 100, by =2.5)
x2
x2[1:10]
x2[20:30]
length(x2)-1 # second last value
x2[-1] # all but first position
position <- c(1,5,10) # create a vector with intersting positions
x2[position]
x2 > 10
x2[x2 <= 10 | x2 >= 15] # gives actual data that fits to the condition
(x2 <= 10) | (x2 >= 15) # gives the positions that fit to the condition

x2 <- numeric(length(x2))
x2[x2 <= 30] <- 1 # set these values to 1

x2[(x2 > 30)&(x2 < 70)] <- 2 # set all these values to 2

# faster possibility to change ceveral values at the same time
library(car)
x2 <- recode(x2, "0:30=1; 30:70=2; else=3")


summary(x2)
sum(x2)
cumsum(x2) # cumulativ sum.

rev(x2) # reverts the order
sort(x2, decreasing = TRUE)
sample(x2, 10) # takes samples

test <- data.frame(A = c(1,2,3), B=c("aB1", "aB2", "aB3")) # create a data frame.
test

test[, "A"]
test$A



df <- data.frame(plot = "location_name_1", measure1 = runif(100) * 1000, measure2 = round(runif(100) *100), 
                 value = rnorm(100, 2, 1), ID = rep(LETTERS, 100))
df_2 <- data.frame(plot="location_name_2", measure1 = runif(50)*100, measure2 = round(runif(50)*10),
                  value = rnorm(50), ID = rep(LETTERS, 50))
df_3 = rbind(df, df_2)

df_3[100:115, c("plot", "measure1", "measure2")]
df_3[df_3$value > 3.0, ]
df_3[df_3$value <=3 & df_3$value >= 2, ]

df_3$new_col <- df_3$measure1*df_3$measure2






