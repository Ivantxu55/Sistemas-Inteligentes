# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)
library(rlist)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/informed/hill-climbing-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# One Pizza
source("../problem/one-pizza-complete.R")

# Executes hill climbing search and return the results
execute.hill.climbing <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename)
  return(hill.climbing.search(problem = problem, max_iterations = 50, count_print = 10, trace = FALSE))
}

# Execute an algorithm several times and analyze results
test.algorithm <- function(filename, algorithms, times) {
  results     <- vector(mode = "list", length = times * length(algorithms))
  result_pos  <- 1;

  #Execute each algorithm N times
  for (algorithm in algorithms) {
    # Initialize a problem instance for the analysis
    problem <- initialize.problem(filename)

    print(paste0("Executing: '", algorithm, ", ", times, " times with '",
                 problem$name, "'"), quote = FALSE)

    for (i in 1:times) {
      results[[result_pos]] <- execute.hill.climbing(filename)
      result_pos <- result_pos + 1
    }
  }

  # Analyze results
  results_df <- local.analyze.results(results, problem)

  print(paste0("Best cost: ", round(max(results_df$Cost), 2),
               " - Mean: ", round(mean(results_df$Cost), 2),
               " - SD: ", round(sd(results_df$Cost), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2),
               " - Mean: ", round(mean(results_df$Runtime), 2),
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)

  # Print results in an HTML Table
  kable_material(kbl(results_df, caption = paste(problem$name, algorithm, sep = " - ")),
                 c("striped", "hover", "condensed", "responsive"))
}

# Clear console
cat("\014")
graphics.off()

algorithms    <- vector(mode = "list")
algorithms[1] <- "hill.climbing"

filenames     <- vector(mode = "list")
filenames[1]  <- "../data/one-pizza/a_an_example.in.txt"
filenames[2]  <- "../data/one-pizza/b_basic.in.txt"
filenames[3]  <- "../data/one-pizza/c_coarse.in.txt"
filenames[4]  <- "../data/one-pizza/s_spanish.in.txt"
filenames[5]  <- "../data/one-pizza/d_difficult.in.txt"
filenames[6]  <- "../data/one-pizza/e_elaborate.in.txt"

# Number of times to execute each algorithm
times       <- 10
# Execute several times each algorithm for a specific problem
test.algorithm(filenames[[4]], algorithms, times)
