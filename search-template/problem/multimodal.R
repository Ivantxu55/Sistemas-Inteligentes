# =========================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =========================================================================

rm(list = ls()) # Clear Environment
cat("\014")     # Clear Console
graphics.off()
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
file <- "../data/multimodal-planner/map4.txt"

obtener_datos_transporte <- function(skip) {
  transporte <- list()
  # Leer las primeras "skip" líneas del archivo CSV
  n <- read.csv(file, header = FALSE, sep = ";", skip = skip, nrows = 1)
  t <- (str_split_fixed(n[1], ":", 2))
  tiempo <- as.numeric(t[2])
  nombre <- t[1]
  coste <- as.numeric(n[2])
  n[1] <- NULL
  n[1] <- NULL
  
  posiciones <- str_split_fixed(n, ",", 2)
  
  transporte <- list(nombre = nombre,
                tiempo = tiempo,
                coste = coste,
                posiciones = posiciones)
  
  # Regresar la lista
  return(transporte)
}
# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
#initialize.problem <- function(file) {
  problem <- list() # Default value is an empty list.
  
  # This attributes are compulsory
   problem$name              <- "Multimodal"
   start <- read.csv(file, header = FALSE, sep = ";", skip = 1, nrows = 1)
   start <- as.numeric(str_split_fixed(start, ",", 2))
                       
   problem$state_initial     <- list(posicion = start,
                                     tiempo = 0, 
                                     ticket = c(0, 0, 0), 
                                     transporte = 0)
   final <- read.csv(file, header = FALSE, sep = ";", skip = 2, nrows = 1)
   final <- as.numeric(str_split_fixed(final, ",", 2))
   
   problem$final       <- final
   
   size  <- read.csv(file, header = FALSE, sep = ";", nrows = 1)
   size  <- as.numeric(str_split_fixed(size, ",", 2))
   problem$size             <- size
   problem$walk            <- list(nombre = "",
                                  tiempo = 0,
                                  coste = 0 )
   n <- read.csv(file, header = FALSE, sep = ";", skip = 3, nrows = 1)
   
   t <- as.numeric(str_split_fixed(n[1], ":", 2))
   problem$walk <- list(nombre = "Walk",
                        tiempo = t[2],
                        coste = as.numeric(n[2]))
   n <- read.csv(file, header = FALSE, sep = ";", skip = 4, nrows = 1)
   
   t <- as.numeric(str_split_fixed(n[1], ":", 2))
   problem$exchange <- list(nombre = "Ex",
                            tiempo = t[2],
                            coste = as.numeric(n[2]))
   n <- read.csv(file, header = FALSE, sep = "\n")
   num <- as.numeric(nrow(n))
   if(num > 5){
    i <- 5
    transportes <- c()
    a <- 6
    for(i in 5:num){
      problem[[a]] <- obtener_datos_transporte(i)
      
      a <- a+1
      #Hacemos un vector de vectores para las posiciones de las paradas de metro
   #Falta hacer los splits del n para sacar las posiciones de las paradas de metro
    }
   
   }
  # You can add additional attributes
    problem$posible_actions <- data.frame(dierction = c("N", "S", "E", "W", "NE", "NW", "SE", "SW",
                                                        "Walk", "Metro", "Bus", "Tren"))
   
  
  #return(problem)
#}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  if(action == "Walk" && !state$transporte == 0)
    return(TRUE)
  if(action == "Metro" && !state$transporte == 1 && state$posicion %in% problem$transporte$posiciones)
    return(TRUE)
  if(action == "Bus" && !state$transporte == 2)
    return(TRUE)
  
  
  
  result <- FALSE # Default value is FALSE.
  
  # <INSERT CODE HERE TO CHECK THE APPLICABILITY OF EACH ACTION>
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  # <INSERT YOUR CODE HERE TO MODIFY CURRENT STATE>
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  if(state$posicion == problem$final)
    result <- TRUE
  else result <- FALSE # Default value is FALSE.
  
  # <INSERT YOUR CODE HERE TO CHECK WHETHER A STATE IS FINAL OR NOT> 
  
  return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  # <INSERT YOUR CODE HERE TO GENERATE A STRING THAT REPRESENTS THE STATE> 
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE> 
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
	return(1) # Default value is 1.
}
