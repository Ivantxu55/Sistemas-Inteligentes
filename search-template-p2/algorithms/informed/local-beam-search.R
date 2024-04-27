local.beam.search = function(problem,
                             max_iterations = 1000, 
                             count_print = 100, 
                             trace = FALSE,
                             beams = 3) {
  
  name_method      <- paste0("Local Beam Search")
  state_initial    <- list(problem$state_initial)
  
  for (i in 1:beams){
    state_initial[[i]] <- sample(c(1:problem$container_size), problem$p)
  }
  

  
  actions_possible <- problem$actions_possible
  start_time       <- Sys.time()
  
  current_nodes <- list()
  
  for (i in 1:beams){
    node_current <- list(parent = c(),
                         state = state_initial[[i]],
                         actions = c(),
                         depth = 1,
                         cost = get.cost(state = state_initial[[i]], problem = problem),
                         evaluation = get.evaluation(state_initial[[i]], problem))
    current_nodes[[i]] <- node_current
  }
  
  count <- 1
  end_reason <- 0
  
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  
  while (count <= max_iterations) {
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", Current node=", current_nodes[[1]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
    }
    
    if (trace) {
      print(paste0("Current node=",  current_nodes[[1]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
      to.string(state =  current_nodes[[1]]$state, problem = problem)
    }
    
    sucessor_nodes_beam <- list()
    for (i in current_nodes){
      sucessor_nodes <- local.expand.node(i, actions_possible, problem)
      sucessor_nodes_beam <- append(sucessor_nodes_beam, sucessor_nodes)
    }
    
    sucessor_nodes_beam <- sucessor_nodes_beam[order(sapply(sucessor_nodes_beam,function (x) x$evaluation))]
    sucessor_nodes_beam <- sucessor_nodes_beam[1:beams]
    
    current_nodes <- current_nodes[order(sapply(current_nodes,function (x) x$evaluation))]
    
    bestNode <- sucessor_nodes_beam[[1]]
    
    if (bestNode$evaluation <= current_nodes[[1]]$evaluation) {
      current_nodes <- sucessor_nodes_beam
      
      if (trace){
        print(paste0("New current node=", current_nodes[[1]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
        to.string(state = current_nodes[[1]]$state, problem = problem)
      }
    } else {
      end_reason <- "Local_Best"
      
      report <- rbind(report, data.frame(iteration = count,
                                         nodes_frontier = 1,
                                         depth_of_expanded = current_nodes[[1]]$depth,
                                         nodes_added_frontier = 1))
      
      break
    }
    
    report <- rbind(report, data.frame(iteration = count,
                                       nodes_frontier = 1,
                                       depth_of_expanded = current_nodes[[1]]$depth,
                                       nodes_added_frontier = 1))
    count <- count + 1
  }
  
  end_time       <- Sys.time()
  
  result         <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  if (end_reason == "Local_Best") {
    print("Local best found!!", quote = FALSE)
  } else {
    print("Maximum iterations reached", quote = FALSE)
  }
  
  print(to.string(state = current_nodes[[1]]$state, problem = problem))
  
  result$state_final <- current_nodes[[1]]
  result$report      <- report
  
  return(result)
}
