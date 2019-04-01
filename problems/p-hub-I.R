# =======================================================================
# Names: Aleksander Bobiński, Karolina Palka, Maciej Stokfisz
# Group Number: Group I
# Assignment:
# Date:
# =======================================================================
# 1. Be sure to include, with this template, any necessary files
#    for execution, including datasets (problem.R, methodXXX.R, ...)
#    (submission of the entire template folder is recommended)
# 2. If you use a function of a certain package, do not forget to include the
#    corresponding call to the "library ()" function
# 3. Do not forget to comment on the code, especially those non-trivial commands
#    (remember that part of the rating depends on the cleaning of the code)
# 4. It is strongly recommended to test any implemented function in order to
#    check for its proper operation
# =======================================================================
# (This is a general code, you must adapt it)
# =======================================================================

# This function must return a list with the information needed to
# solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem = function(phub.file.name){
  problem = list()
  problem$distances <- as.matrix(read.csv(phub.file.name, header = FALSE, skip = 2, sep = " "))
  data.file <- file(phub.file.name, open = "r")
  problem$number.of.hubs <- as.numeric(readLines(data.file, n = 1))
  close(data.file)
  problem$state.initial <- 2:(problem$number.of.hubs+1)
  problem$actions.possible <- do.call(
    expand.grid,
    replicate(problem$number.of.hubs, -1:1, simplify = FALSE)
  )
  problem$name = "phub-I"
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable <- function (state, action, problem){
  # TODO(Aleksander Bobiński) The unlist() part needs an additional test
  future.state <- state + as.vector(unlist(action))
  return (!(anyDuplicated(future.state) || 0 %in% future.state || (nrow(problem$distances) + 1) %in% future.state))
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect <- function (state, action){
  return (state + as.vector(unlist(action)))
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited
is.final.state = function (state, finalstate = NULL){
  return(identical(state, finalstate))
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  print(as.vector(unlist(state)))
}

# =======================================================================
# Return the cost of applying an action over a state
get.cost = function (action,state){
  # Return the cost of applying an action over a state
  return(1)
}

# =======================================================================
# (Used for Informed Algorithms)
# Heuristic function used in Informed algorithms
get.evaluation = function(state,problem){
  updated.distances <- problem$distances
  hub.indeces <- state
  
  hub.distances <- sapply(hub.indeces, function (index) {
    problem$distances[, index]
  })
  
  closest.hub.indeces.indeces <- sapply(1:nrow(hub.distances), function (row.index) {
    which.min(hub.distances[row.index, ])
  })
  
  for(row.index in 1:nrow(problem$distances)) {
    for(column.index in row.index:ncol(problem$distances)) {
      closest.A.hub <- hub.indeces[closest.hub.indeces.indeces[row.index]]
      closest.B.hub <- hub.indeces[closest.hub.indeces.indeces[column.index]]
      updated.distances[row.index, column.index] <- 
        problem$distances[closest.A.hub, column.index] + 
        problem$distances[row.index, closest.B.hub] +
        problem$distances[closest.A.hub, closest.B.hub]
    }
  }
  
	return(sum(updated.distances[upper.tri(updated.distances)]))
}
