# Install required packages
library(stringr)

# This function must return a list with the information needed to solve the problem.
initialize.problem <- function(file, random_actions = FALSE) {
  problem <- list()
  problem$name  <- paste0("Feet Maze - [", file, "]")
  # Size of the matrix: rows, columns
  problem$size <- read.csv(file, header = FALSE, nrows = 1)
  problem$size <- as.numeric(str_split_fixed(problem$size, ";", 2))
  # Matrix of feet
  problem$maze <- read.csv(file, header = FALSE, skip = 1, nrows = problem$size[1], sep=";")
  # Initial State: X = column, Y = row
  problem$state_initial <- read.csv(file, header = FALSE, skip = problem$size[1]+1, nrows = 1, sep = ",")
  problem$state_initial <- as.numeric(str_split_fixed(problem$state_initial, ",", 1))
  problem$state_initial <- problem$state_initial+1 #Adapt values to matrix indexes
  # Final State: X = column, Y = row
  problem$state_final <- read.csv(file, header = FALSE, skip = problem$size[1]+2, nrows = 1, sep = ",")
  problem$state_final <- as.numeric(str_split_fixed(problem$state_final, ",", 1))
  problem$state_final <- problem$state_final+1 #Adapt values to matrix indexes
  # Left walls: "x,y" strings
  problem$walls_left <- read.csv(file, header = FALSE, skip = problem$size[1]+3, nrows = 1, sep = ";")
  # Right walls: "x,y" strings
  problem$walls_right <- read.csv(file, header = FALSE, skip = problem$size[1]+4, nrows = 1, sep = ";")
  # Down walls: "x,y" strings
  problem$walls_down <- read.csv(file, header = FALSE, skip = problem$size[1]+5, nrows = 1, sep = ";")
  # Top walls: "x,y" strings
  problem$walls_top <- read.csv(file, header = FALSE, skip = problem$size[1]+6, nrows = 1, sep = ";")  

  # There are 4 actions
  directions <- c("UP", "DOWN", "LEFT", "RIGHT")
  
  if (random_actions) {
    directions <- sample(directions)
  }
  
  problem$actions_possible <- data.frame(direction = directions, stringsAsFactors = FALSE)

  return(problem)
}

# Analyzes if an action can be applied in a state.
is.applicable <- function (state, action, problem) {
  #(1) the boundaries of the maze are not exceeded
  #(2) there is no wall between current location and new location
  #(3) the foot of the new location is different from the current one.
  
  current_foot <- problem$maze[state[2], state[1]]

  if (action == "UP" & state[2] > 1) {
    location_current  <- paste(state[1]-1, state[2]-1, sep=",")
    location_new      <- paste(state[1]-1, state[2]-2, sep=",")
    #Check walls: current location TOP or new location DOWN
    are_walls <- location_current %in% problem$walls_top | 
                 location_new %in% problem$walls_down
    #Check, walls and opposite foot
    return (!are_walls & problem$maze[state[2]-1, state[1]] != current_foot)
  }
  
  if (action == "DOWN" & state[2] < problem$size[1]) {
    location_current  <- paste(state[1]-1, state[2]-1, sep=",")
    location_new      <- paste(state[1]-1, state[2], sep=",")
    #Check walls: current location DOWN or new location TOP
    are_walls <- location_current %in% problem$walls_down | 
                 location_new %in% problem$walls_top
    #Check walls and opposite foot
    return (!are_walls & problem$maze[state[2]+1, state[1]] != current_foot)
  }
  
  if (action == "LEFT" & state[1] > 1) {
    location_current  <- paste(state[1]-1, state[2]-1, sep=",")
    location_new      <- paste(state[1]-2, state[2]-1, sep=",")
    #Check walls: current location LEFT or new location RIGHT
    are_walls <- location_current %in% problem$walls_left | 
                 location_new %in% problem$walls_right
    #Check walls and opposite foot
    return (!are_walls & problem$maze[state[2], state[1]-1] != current_foot)
  }
  
  if (action == "RIGHT" & state[1] < problem$size[2]) {
    location_current  <- paste(state[1]-1, state[2]-1, sep=",")
    location_new      <- paste(state[1], state[2]-1, sep=",")
    #Check walls: current location RIGHT or new location LEFT
    are_walls <- location_current %in% problem$walls_right | 
                 location_new %in% problem$walls_left
    #Check walls and opposite foot
    return (!are_walls & problem$maze[state[2], state[1]+1] != current_foot)
  }
  
  return (FALSE)
}

# Returns the state resulting on applying the action over the state.
effect <- function (state, action, problem) {
  result <- state
  
  if (action == "UP") {
    result[2] <- result[2]-1
  }
  
  if (action == "DOWN") {
    result[2] <- result[2]+1
  }
  
  if (action == "LEFT") {
    result[1] <- result[1]-1
  }
  
  if (action == "RIGHT") {
    result[1] <- result[1]+1
  }

  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(all(state == problem$state_final))
}

# Transforms a state into a string
to.string <- function (state, problem) {
  return(paste0("[x=", state[1], ", y=", state[2], "]"))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  # Manhattan distance
  return(abs(state[1] - problem$state_final[1]) + abs(state[2] - problem$state_final[2]))
}
