# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/informed/hill-climbing-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/p-hub-problem.R")

# Executes hill climbing search and return the results
execute.hill.climbing <- function(filename, p = 2) {
  # Initialize problem
  problem <- initialize.problem(p = p, filename = filename)
  return(hill.climbing.search(problem = problem))
}

# Execute an algorithm several times and analyze results
test.algorithm <- function(filename, p = 2, times, algorithms) {
  results     <- vector(mode = "list", length = times * length(algorithms))
  result_pos  <- 1;
  
  #Execute each algorithm N times
  for (algorithm in algorithms) {
    # Initialize a problem instance for the analysis
    problem <- initialize.problem(filename, p)
    
    print(paste0("Executing: '", algorithm, ", ", times, " times to '", 
                 problem$name, "'"), quote = FALSE)
    
    for (i in 1:times) {
      results[[result_pos]] <- execute.hill.climbing(filename)
      result_pos <- result_pos + 1
    }
  }
  
  # Analyze results
  results_df <- local.analyze.results(results, problem)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2), 
               " - Mean: ", round(mean(results_df$Evaluation), 2), 
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
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
filenames[1]  <- "../data/p-hub/AP40.txt"
filenames[2]  <- "../data/p-hub/AP100.txt"

# Number of times to execute each algorithm
times       <- 5
# Execute several times the algorithms for a given problem
test.algorithm(filename = filenames[[1]], algorithms = algorithms, times = times)
# Execute several times the algorithms for a given problem
test.algorithm(filename = filenames[[2]], algorithms = algorithms, times = times)
