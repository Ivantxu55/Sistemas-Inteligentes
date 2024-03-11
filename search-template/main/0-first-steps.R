# R Script for Introducing Basic Concepts to Java Programmers
# Created by Onieva, E. & Carballedo, R.
# Revised using ChatGPT 3.5 and GitHub Copilot

# You can execute all the script using the "Source" button or pressing "Ctrl + Shift + S"
# You can execute a single line or a block of code using "Ctrl + Enter"
# In MacOs use "Cmd" instead of "Ctrl"

# Clearing the workspace and console
rm(list = ls()) # Clear Environment
cat("\014")     # Clear Console
graphics.off()  # Clear Plots

# Setting the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This command can give an error if "non-ASCII" characters are in the path.
# Is so, use the menu option "Sessions -> Set Working Directory-> To Source File Location"

# c() : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/c
# funciton c() declares a vector or array of values
vec <- c(1, 2, 3, 4, 5, 6)
# When you write the name of a variable the value is printed in the Console
vec

# Boolean types are defined either with the first letter T / F or the complete word TRUE / FALSE.
vec <- c(TRUE, T, FALSE, F)
vec <- c("one", "two", "three")

# There are many ways to create and initialize vectors (and matrices)
vec_1 <- 1:10 # Numbers from 1 to 10

# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/seq
vec_2 <- seq(1, 10, 2) # Sequence from 1 to 10 adding 2 in each step 

# https://www.rdocumentation.org/packages/compositions/versions/2.0-6/topics/runif
vec_3 <- runif(10, 50, 100) # 10 random numbers between 50 and 100

# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/rep
vec_4 <- rep(c(1, 2), 10) # Repeat 10 times the content of a vector

# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sample
vec_5 <- sample(1:10) # Generate a vector with all the numbers between 1 and 10 in a random order

# https://www.rdocumentation.org/packages/gtools/versions/3.9.5/topics/combinations
library(gtools)
vec_6 <- combinations(3, 2, v = 1:3, repeats.allowed = FALSE)
vec_7 <- permutations(3, 2, v = 1:3, repeats.allowed = FALSE)

# matrix() : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/matrix
# matrix() creates a martix. The "=" operator assigns values to parameters in a function.
mat <- matrix(0, nrow = 5, ncol = 3)
mat

# We access vectors and matrices (rows an columns) using brackets []
vec[2] # Access to the second element of vec
mat[1, 3] # Access to the element in the first row and third column of mat

# For matrices, we can access an entire row/column leaving in blank the column or row
mat[1, ] # Access to the first row of mat
mat[, 1] # Access to the first column of mat

# We can modify multiple elements of a matrix in a single operation
mat[1, ] <- 10 # Set to 10 all the elements of row 1.
mat

mat[, 2] <- mat[, 2] + 10 # Add 10 to all the elements of column 2.
mat

# Square all the elements of vec_2 
vec_2
vec_2 <- vec_2 ^ 2 # Square all the elements of vec_2
vec_2

# We can also use FOR sentence (there are more efficent ways)
# nrow() function returns the number of rows of a matrix
for (i in 1 : nrow(mat)) {
  mat[i, 3] <- mat[i, 3] + 1 # Add 1 to all the elements of column 3.
}

mat

# apply() : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/apply
# The second parameter defines if the function is applied on rows ('1') or clumns ('2').
apply(mat, 1, mean) # Calculate the mean of all rows (1 define row).
apply(mat, 2, mean) # Calculate the mean of all columns (2 define columns).
apply(mat, c(1, 2), mean) # Calculate the mean of all elements in the matrix.

# Definition of a function.
test.function <- function(x) {
  value <- x[2] ^ 2 - x[3] # Square the second element and substract the third element.
  return(value)
}

# Invoke the function with a vector as parameter
test.function(c(1, 3, 5))
#apply() is similar to Java's foreach() method [using lambda expressions].
mat[, 1] <- apply(mat, 1, function(x) test.function(x))
mat

# Making validations or conditional changes on a vector (or matrix)
vec <- sample(1:10) # Create a vector of numbers between 1 and 10 and order randomly.
vec
vec < 5            # Check if all the elements are lower than 5.

# %in% operator check if a value is in a collection
result <- 5 %in% vec # Check if 5 is in vec
result

#Checks whether each element of the auxiliary vector is in vec. 
#Returns a boolean value for each element of the auxiliary vector.
result <- c(1, 2, 3) %in% vec # Check if 1, 2 and 3 are in vec
result

#Checks if any element of the auxiliary vector is in vec.
#Returns a single boolean value.
result <- any(c(3, 6, 8) %in% vec) # Check if any of 3, 6 and 8 are in vec
result

#Checks if all the elements of the auxiliary vector are in vec.
#Returns a single boolean value.
result <- all(c(3, 6, 8) %in% vec) # Check if all of 3, 6 and 8 are in vec
result

#Checks if no element of the auxiliary vector is in vec.
#Returns a single boolean value.
result <- !all(c(3, 6, 8) %in% vec) # Check if none of 3, 6 and 8 are in vec
result

# which() : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/which
which(vec < 5) # Get the indexes with value lower than 5.

# any(): https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/any
any(vec < 5) # Check if any value of the vector is less than 5   

vec[vec < 5] <- 0  # Set to 0 elements lower than 5.
vec

mat

# Get the index of all the elements with value 1.
which(mat == 1) 
# Return the correlative index starting at position [1,1], [1,2], [1,3] ...
which(mat == 1, arr.ind = TRUE) # Get the row,col of all the elements with value 1
mat[mat == 1] <- 0 # Set to 0 all the element with value 1
mat

# Create lists with named elements
person1 <- list()                              # Create an (empty) list
person1$name   <- "Mike"                       # "name" contains a string
person1$age    <- 29                           # "age" contains a number
person1$cities <- c("Donosti", "Bilbao")       # "cities" contains a vector of strings
person1$mat    <- matrix(0, nrow = 3, ncol = 3)   # "mat" contains a matrix

person2 <- list(name = "Mary", 
                age = 30, 
                cities = c("Donosti", "Bilbao", "Madrid"), 
                mat = matrix(0, nrow = 3, ncol = 3))

# Accessing to elements in lists. Double bracket operator is used [[]]
person2$name # value of "name" on item2
person2[[1]] # 1st element of item2
person2[[3]][2] # 2nd element of the 3rd element on item2

# Lists of lists (in this case, list of persons)
persons <- list(person1, person2)
persons[[1]]$cities[1] # 1st city of the 1st person in the list
