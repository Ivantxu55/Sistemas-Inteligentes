
random.restart.hill.climbing = function(problem,
                                      restarts = 100,
                                      max_iterations = 1000, 
                                      count_print = 100, 
                                      trace = FALSE, 
                                      graph_search = FALSE,
                                      max_depth = 20) {
  # Get Start time
  print(paste0("* START: Iterative Deeping Search"), quote = F)
  start_time       <- Sys.time()
  
  limit <- 0
  
  temp <- 100000
  for (i in 1:restarts) {
    cat(paste0("Restart: ", i, " - "))
    hill_climbing_result <- hill.climbing.search(problem,
                                                max_iterations = max_iterations,
                                                count_print = count_print, 
                                                trace = trace
                                                )
    if (temp > hill_climbing_result$state_final$evaluation) {
      temp <- hill_climbing_result$state_final$evaluation
      best_hill_cimbing_result <- hill_climbing_result
    }
   }
  
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name        <- paste0("Random Restart Hill Climbing Search")
  result$runtime     <- end_time - start_time
  result$state_final <- best_hill_cimbing_result$state_final
  result$report      <- best_hill_cimbing_result$report
  print(paste0("* END: ", result$name), quote = F)
  
  return(result)
}
