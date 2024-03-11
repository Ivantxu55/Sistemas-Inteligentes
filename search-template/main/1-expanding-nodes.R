# Clear environment and console
rm(list=ls())
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#---------------------------------------------------------------------------
# The problem "Wolf, goat, and cabbage"
# https://en.wikipedia.org/wiki/River_crossing_puzzle
# A farmer must transport a Wolf, a goat and cabbage from one side of a river 
# to another using a boat which can only hold one item in addition to the farmer, 
# subject to the constraints that the wolf cannot be left alone with the goat, 
# and the goat cannot be left alone with the cabbage

#---------------------------------------------------------------------------
# State: the state will be defined as a 4-elements (boolean) vector,
# representing each one of them the position of the elements:
# 1st position: location of the farmer  (TRUE: left / FALSE: right)
# 2nd position: location of the wolf    (TRUE: left / FALSE: right)
# 3rd position: location of the goat    (TRUE: left / FALSE: right)
# 4th position: location of the cabbage (TRUE: left / FALSE: right)

# The initial state is a data.frame with the elements in the initial side
state_initial <- data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
print(state_initial)

# Create several states
state1 <- data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
state2 <- data.frame(farmer = FALSE, wolf = TRUE, goat = FALSE, cabbage = TRUE)
state3 <- data.frame(farmer = FALSE, wolf = FALSE, goat = FALSE, cabbage = FALSE)

# The final state test can be done by checking that all the elements are FALSE
(!state1$farmer && !state1$wolf && !state1$goat && !state1$cabbage)
(!state2$farmer && !state2$wolf && !state2$goat && !state2$cabbage)
(!state3$farmer && !state3$wolf && !state3$goat && !state3$cabbage)

# Or, in a simpler way, adding the elements in a vector and comparing with 0
sum(as.numeric(state1)) == 0
sum(as.numeric(state2)) == 0
sum(as.numeric(state3)) == 0

# Or, checking if all of the elements of the data.frame are equal to FALSE
all(state1 == FALSE)
all(state2 == FALSE)
all(state3 == FALSE)

#---------------------------------------------------------------------------
# Actions: actions with their conditions and effects
# 4 different actions can be done
# 1: "farmer"  -> changes the side of the farmer alone
# 2: "wolf"    -> changes the side of the wolf and the farmer
# 3: "goat"    -> changes the side of the goat and the farmer
# 4: "cabbage" -> changes the side of the cabbage and the farmer

# 1: moving the farmer can be done if no restrictions are violated 
# 2-4: moving another element can be done if no restrictions are violated and 
# the element is in the same side of the river than the farmer

# Farmer can be moved if:
# - wolf and goat are not in the same side AND
# - goat and cabbage are not in the same side
(state1$wolf != state1$goat) && (state1$goat != state1$cabbage)
(state2$wolf != state2$goat) && (state2$goat != state2$cabbage)

# Goat can be moved if it is in the same side of the river than the farmer
(state1$farmer == state1$goat)
(state2$farmer == state2$goat)

# Cabbage can be moved if:
# - cabbage and the farmer are in the same side of the river AND
# - wolf and goat are not in the same side
(state1$wolf != state1$goat) && (state1$cabbage == state1$farmer)
(state2$wolf != state2$goat) && (state2$cabbage == state2$farmer)

# The effect of applying an action (goat, for instance) is to create a new state
# with the farmer and the element in the opposite side of the river
new_state <- state2
new_state$farmer <- !state2$farmer # Change the side of the farmer
new_state$goat <- !state2$goat # Change the side of the goat
new_state

# Let's solve the problem
rm(list=ls())
cat("\014")

# Import the problem formulation from a file
source("../problem/river-crossing-puzzle.R")

# Initialize the problem get the initial state and the actions
problem <- initialize.problem()
state   <- problem$state_initial
actions <- problem$actions_possible

# Initialize the frontier list with the initial state
frontier <- list(state)
print(paste0("Frontier: ", frontier), quote = FALSE)

# Extract and remove the first state from the frontier list
current_state <- frontier[[1]]
frontier[[1]] <- NULL

# Check if the current state is the final state
is.final.state(current_state)

# Create a data.frame with all the applicable actions over the current.state
applicable_actions <- sapply(actions$action, function (x) is.applicable(current_state, x, problem))
print(applicable_actions)

# Expand current_state: Apply all applicable actions and generate sucessor states
res <- lapply(actions$action[applicable_actions], function(x) effect(current_state, x))
print(res)

# Add successors states to the frontier list
frontier <- append(frontier, res)
print(paste0("Frontier: ", frontier), quote = FALSE)

# AND REPEAT THE PROCEDURE... UNTIL FINAL STATE FOUND!!!
