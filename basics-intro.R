# basics-intro.R
# Author: Cait Harrigan
# Date: November 2019
# Version 1.0

# Objects

# Numbers
(0.013 + 0.008 + 0.011) / 3

# Variables

num_snails <- 3

speed_snail1 <- 0.013
speed_snail2 <- 0.008
speed_snail3 <- 0.011

(speed_snail1 + speed_snail2 + speed_snail3) / num_snails


# Data Structures
# After each variable assignment, write down what you think you'll see,
# then print the true value

# Variables
# Numbers

# Prediction: print(a) will show 1
a <- 1
print(a)

# Strings (of characters)
# Prediction: 
b <- "my snail is the fastest!"
print(b)

# c() is a function that makes a vector
# Prediction:
snail_speeds <- c(speed_snail1, speed_snail2, speed_snail3)
print(snail_speeds)

# Functions
# c() is a function, and it returns a vector
# we can use the mean() function to get the average of the vector
mean(snail_speeds)

# we can even create our own functions
fastest_snail <- function(speeds){
  
  snail_num <- which(speeds == max(speeds))
  message <- sprintf("The fastest is snail number %s", snail_num)
  
  return(message)
}

fastest_snail(snail_speeds)

# functions also have documentation that is accessible by using ? or 
# the help funciton help()
?mean
help(mean)

# More types of objects

# We saw numbers and strings
print(a)
print(b)

# Booleans 
print(TRUE == TRUE)
print(TRUE == FALSE)
print(TRUE != FALSE)
print(7 < 8)

# Vectors
# Prediction: 
c <- 1:5
print(c)

# Factors (for categorical variables)
# Prediction: 
shell_snail1 <- factor("brown", levels = c("brown", "orange"))
print(shell_snail1)

# Lists
# Prediction: 
d <- list(1,2,3,4,5)
print(d)

# Prediction: 
e <- list(b, "it came in ", a, "st place")
print(e)

# Prediction:
f <- unlist(e)
print(f)

# Prediction:
g <- paste(f, collapse = " ")
print(g)

# Dataframes
# Very useful for storing multiple pieces of data together. 

print(snail_speeds)

# We want shell colour as a categorical variable
shell_snail1 <- "brown"
shell_snail2 <- "orange"
shell_snail3 <- "brown"

snail_shells <- c(shell_snail1, shell_snail2, shell_snail3)

# What should we put inside the as.factor() function? Replace the string below.
snail_shells_factor <- as.factor("replace this")
print(snail_shells_factor)

snail_data <- data.frame(speed = snail_speeds, shell_colour = snail_shells_factor)
print(snail_data)




# Matrices are to dataframes as lists are to vectors. Similar in appearance, with 
# some usage differences. A matrix can have a matrix inside it, in the way that 
# a list of lists is possible. 
# cbind is for column bind, rbind is for row bind

h <- cbind(snail_speeds, snail_shells)
print(h)

# accessing data (indexing)
# data can be accessed out of datastructures, like lists, vectors, dataframes, matricies
# single [] returns an object of the same type, double [[]] may returns an object of a different type.
print(snail_data)
print(snail_data[1])
print(snail_data[[1]])

# we can keep accessing within the return object
print(snail_data[[1]][2])

# We can also view columns of the dataframe by name. Can index with $ or [[]]
print(snail_data$speed)
print(snail_data[["speed"]])

# This takes a little practice to get used to! Smart indexing and subsetting 
# yield elegant solutions. 

# plotting
# the ~ operator lets us write formulas. This is great for plotting
group_averages <- aggregate(speed ~ shell_colour, data = snail_data, FUN = mean)
barplot(group_averages$speed)

# and we can add extra parameters for nice looking plots
barplot(group_averages$speed, col = levels(group_averages$shell_colour),
        main = "Average snail speed", ylab = "m/s", names.arg = c("brown shell", "orange shell"))


# Scripts

# up until now, this .R file is not a useful script. You can press "run", but you
# won't see anything happen. This isn't because nothing's happening, but rather the 
# results are not being output to somewhere you can see. To fix this, uncomment the 
# next line to save the plot to an image file, and try running it again.

png()
barplot(group_averages$speed, col = levels(group_averages$shell_colour),
        main = "Average snail speed", ylab = "m/s", names.arg = c("brown shell", "orange shell"))
dev.off()

# there are two resources to know for getting the most out of R - CRAN and Bioconductor.
# Both of these have lots of different packages available, which give you access
# to functionality that other people have developed. To name a few...
# ggplot2
# dplyr
# BSgenome
# biomaRt
# 

# [END]
