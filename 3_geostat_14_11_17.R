# Course Script 14_11_17

setwd("C:/Users/s331737/Desktop/Lucia Tischer")

## 1) repetion ####
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
