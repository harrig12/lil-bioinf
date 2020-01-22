# basics-intro.R
# Author: Cait Harrigan
# Date: Jan 2020
# Version 1.0

# Objects

# Numbers

(0.013 + 0.008 + 0.011) / 3

# Character string

"I like bioinformatics!"

strsplit("I like bioinformatics!", split= "")

# Variables
num_snails <- 3

speed_snail1 <- 0.013
speed_snail2 <- 0.008
speed_snail3 <- 0.011

(speed_snail1 + speed_snail2 + speed_snail3) / num_snails

# After each variable assignment, write down what you think you'll see,
# then print the true value

# Prediction: print(a) will show 1
a <- 1
print(a)

# Strings (of characters)
# Prediction: 
b <- "my snail is the fastest!"
print(b)

# Functions

# c() is a function, and it returns a vector
# Prediction:
snail_speeds <- c(speed_snail1, speed_snail2, speed_snail3)
print(snail_speeds)

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

# Factors (for categorical variables)
# Prediction: 
shell_snail1 <- factor("brown", levels = c("brown", "orange"))
print(shell_snail1)

# Vectors
# Prediction: 
c <- 1:5
print(c)

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


# accessing data (subsetting)
# data can be accessed out of datastructures, like lists, vectors, dataframes, matricies
# single [] returns a filtered object of the same type
# double [[]] may return an object of a different type (often a simpler one).
# [[]] always returns a single item
print(snail_data)
print(snail_data[1])
print(snail_data[[1]])

# we can keep accessing within the return object
print(snail_data[[1]][2])

# We can also view columns of the dataframe by name. Can index with $ or [[]]
print(snail_data$speed)
print(snail_data[["speed"]])

# Complare this with column selection by index
print(snail_data[,1])

# This takes a little practice to get used to! Smart indexing and subsetting 
# yield elegant solutions. 

# Whe you're starting out, you may find it easier to use subset, and the dplyr package's filter
?subset
?filter

if (! require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

?filter

# We can subset with a boolean vector. Use TRUE for the rows you want to see!
print(c(TRUE, FALSE, TRUE))
print( snail_data[c(T, F, T) ,] )

print(snail_data$shell_colour == "brown")

print( snail_data[snail_data$shell_colour == "brown" ,] )

# plotting
# the ~ operator lets us write formulas. This is great for plotting
group_averages <- aggregate(speed ~ shell_colour, data = snail_data, FUN = mean)
barplot(group_averages$speed)

# and we can add extra parameters for nice looking plots
barplot(group_averages$speed, col = levels(group_averages$shell_colour),
        main = "Average snail speed", ylab = "m/s", names.arg = c("brown shell", "orange shell"))

if (! require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

g <- ggplot(data = group_averages) 
print(g)

# g with scatter plot (point geometry). Note: no assignment to g object
g + geom_point(mapping = aes(x = shell_colour, y = speed, colour = shell_colour))

# ggplot objects have additive elements. Note: assignment to g object
g <- g + geom_bar(mapping = aes(x = shell_colour, y = speed, fill = shell_colour),
             stat = "identity")
print(g)

# there are lots of themes and colour pallets to play with. ggplot is very flexible.
g <- g + scale_fill_brewer(palette = "Set1")
print(g)

# Adding an element that is already present will override previous settings.
g <- g + scale_fill_brewer(palette = "Accent")
print(g)

g <- g + scale_fill_manual(values = c("#C4961A", "#D16309"))
print(g)

# Scripts

# up until now, this .R file is not a useful script. You can press "source", but you
# won't see anything happen. This isn't because nothing's happening, but rather the 
# results are not being output to somewhere you can see (called side-effects). 
# To fix this, uncomment the next line to save the plot to an image file, and try sourcing it again.

#png(filename = "myBarplot.png")
#barplot(group_averages$speed, col = c("#C4961A", "#D16309"),
#        main = "Average snail speed", ylab = "m/s", names.arg = c("brown shell", "orange shell"))
#dev.off()

# similarly, with ggplot...

#g <- g + ggtitle("Avgerage snail speed by shell colour")
#ggsave(plot = g, filename = "ggImage.png")

# there are two resources to know for getting the most out of R - CRAN and Bioconductor.
# Both of these have lots of different packages available, which give you access
# to functionality that other people have developed. To name a few...
# ggplot2
# dplyr
# BSgenome
# biomaRt


# [END]
