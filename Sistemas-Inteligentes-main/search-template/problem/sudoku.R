# This function must return a list with the information needed to solve the problem.
initialize.problem <- function(file) {
  problem <- list()
  
  # Compulsory attributes
  problem$name             <- paste0("Sudoku - [", file, "]")
  # There is an action for each individual cell
  problem$actions_possible <- data.frame(value = 1:9) 
  # The initial state is the one read from the file
  problem$state_initial    <- read.csv(file, header = FALSE)
  # In this problem final_state is unknown
  
  return(problem)
}

# Analyzes if an action can be applied in a state.
is.applicable <- function (state, action, problem) {
  # The value to put is the action
  value <- action

  # Get the first empty cell
  where_put <- which(state == 0, arr.ind = TRUE)[1, ]
  # Get all the cells of the Sudoku with the value
  where_is  <- which(state == value, arr.ind = TRUE)
  # Check if the value is already in the row
  app_row <- any(where_is[, 1] == where_put[1])
  # Check if the value is already in the column
  app_col <- any(where_is[, 2] == where_put[2])
  # Check if the value is already in the square
  square <- floor((where_put - 0.01) / 3)
  square <- (square * 3) + 1
  square <- state[square[1] : (square[1] + 2), square[2] : (square[2] + 2)]
  app_squ <- any(square == value)
  
  # The action is applicable if the value is not in the row, column or square
  return(!app_row & !app_col & !app_squ)
}

# Returns the state resulting on applying the action over the state.
effect <- function (state, action, problem) {
  result <- state
  
  # Get the first empty cell
  where_put <- which(state == 0, arr.ind = TRUE)[1, ]
  # Put the value in the cell
  result[where_put[1], where_put[2]] <- action
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(length(which(state == 0)) == 0)
}

# Transforms a state into a string
to.string <- function (state, problem) {
  return(generate.sudoku.string(state))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  # The evaluation is the number of empty cells
	return(sum(state == 0, na.rm = TRUE))
}

# Function to generate a Sudoku board string
generate.sudoku.string <- function(sudoku_df) {
  sudoku_string <- ""
  
  # Iterate over the rows of the state  
  for (i in 1:nrow(sudoku_df)) {
    # Iterate over the columns of the state
    for (j in 1:ncol(sudoku_df)) {
      # Add the value of the cell
      sudoku_string <- paste0(sudoku_string, sudoku_df[i, j])
      
      # Add a delimiter between each pair of columns
      if (j %% 3 != 0 && j != ncol(sudoku_df)) {
        sudoku_string <- paste0(sudoku_string, ",")
      }
      
      # Add a delimiter between each trío of columns
      if (j %% 3 == 0 && j != ncol(sudoku_df)) {
        sudoku_string <- paste0(sudoku_string, "|")
      }
    }
    
    # Add a delimiter between each row
    sudoku_string <- paste0(sudoku_string, "||")
  }
  
  return(sudoku_string)
}
