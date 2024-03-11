# This function must return a list with the information needed to solve the problem.
initialize.problem <- function() {
  problem <- list()
  
  # Name of the problem
  problem$name <- paste0("River crossing puzzle")
  # There is an action for the movement of each element from one side to the other
  problem$actions_possible <- data.frame(action = c("farmer", "wolf", "goat", "cabbage"), stringsAsFactors = FALSE)
  
  # The initial state is a data.frame with the elements in the initial side
  problem$state_initial   <- data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
  # For this problem we do not need a final state
  
  return(problem)
}

# Analyzes if an action is applicable over a state
is.applicable <- function (state, action, problem) {
  
  switch(action,
    # Farmer can move if wolf and goat are not together, and goat and cabbage are not together 
    "farmer" = {
      return((state$wolf != state$goat) && (state$goat != state$cabbage))
    },
  
    # Wolf can move if farmer is with it, and goat and cabbage are not together
    "wolf" = {
      return((state$goat != state$cabbage) && (state$farmer == state$wolf))
    },
  
    # Cabbage can move if farmer is with it, and wolf and goat are not together
    "cabbage" = {
      return (state$wolf != state$goat) && (state$farmer == state$cabbage)
    },
    
    # Goat can move if farmer is with it.
    "goat" = {
      return(state$farmer == state$goat)
    },
    
    return(FALSE)
  )
}

# Returns the state resulting on applying the action over the state.
effect <- function (state, action, problem) {
  result <- state
  # The farmer always moves
  result$farmer <- !result$farmer
  
  # The other elements move according to the action
  switch(action,
    "wolf" = {
      result$wolf <- !result$wolf
    },
    
    "goat" = {
      result$goat <- !result$goat
    },
    
    "cabbage" = {
      result$cabbage <- !result$cabbage
    }
  )
  
  return(result)
}


# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(all(state == FALSE))
}

# Transforms a state into a string
to.string <- function (state, problem) {
  return(paste0(unlist(state), collapse = ","))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  return(sum(unlist(state)))
}
