local.beam.search = function(problem,
                             beams = 3,
                             max_iterations = 1000,
                             count_print = 100,
                             trace = FALSE
){
  
  name_method      <- paste0("Local Beam Search")
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  # Get Start time
  print(paste0("* START: ", name_method), quote = F)
  start_time       <- Sys.time()
  
  node_current <- c()
  sucessor_nodes <- c()
  node_best_successor <- c()
  
  #Initialization of the beams
  temp <- 1
  while(temp <= beams){
    state_initial    <- problem$state_initial 
    actions_possible <- problem$actions_possible
    node_current[[temp]] <- list(parent = c(),
                                 state = state_initial,
                                 actions = c(),
                                 depth = 1,
                                 cost = get.cost(state = state_initial, problem = problem),
                                 evaluation = get.evaluation(state_initial, problem)
    )
    
    temp <- temp + 1
  }
  
  #Initialization of information for further analysis
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric()
  )
  
  count <- 1
  while (count <= max_iterations) {
    limiteBeams <- 1
    
    while(limiteBeams <= beams){
      if (count %% count_print == 0) {
        print(paste0("Iteration: ", count, ", Current node=", node_current[[limiteBeams]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
      }
      
      if (trace) {
        print(paste0("Current node=", node_current[[limiteBeams]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
        to.string(state = node_current$state, problem = problem)
      }
      
      # Current node is expanded
      sucessor_nodes[[limiteBeams]] <- local.expand.node(node_current[[limiteBeams]], actions_possible, problem)
      # Successor nodes are sorted ascending order of the evaluation function
      sucessor_nodes[[limiteBeams]] <- sucessor_nodes[[limiteBeams]][order(sapply(sucessor_nodes[[limiteBeams]],function (x) x$evaluation))]
      # Select best successor
      node_best_successor[[limiteBeams]] <- sucessor_nodes[[limiteBeams]][[1]]
      
      # The best successor is better than current node
      if (node_best_successor[[limiteBeams]]$evaluation <= node_current[[limiteBeams]]$evaluation) {
        # Current node is updated
        node_current[[limiteBeams]] <- node_best_successor[[limiteBeams]]
        
        #If "trace" is on, the information of the new current node is displayed
        if (trace){
          print(paste0("New current node=", node_current[[limiteBeams]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
          to.string(state = node_current[[limiteBeams]]$state, problem = problem)
        }
        # Local best found
      } else {
        #Add of information for further analysis
        report <- rbind(report, data.frame(iteration = count,
                                           nodes_frontier = 1,
                                           depth_of_expanded = node_current[[1]]$depth,
                                           nodes_added_frontier = 1))
        
        break
      }
      
      
      limiteBeams <- limiteBeams + 1
    }
    
    
    #Add of information for further analysis
    report <- rbind(report, data.frame(iteration = count,
                                       nodes_frontier = 1,
                                       depth_of_expanded = node_current[[1]]$depth,
                                       nodes_added_frontier = 1))
    count <- count + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  node_current <- node_current[order(sapply(node_current,function (x) x$evaluation))]
  
  print(paste0("* END: ", name_method), quote = F)
  
  result$state_final <- node_current[[1]]
  result$report      <- report
  
  return(result)
}