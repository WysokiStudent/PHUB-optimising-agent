expand.node = function(node, actions.possible){
  newnodes = list()
  for (i in 1:nrow(actions.possible)){
    action = actions.possible[i, ]
    state = node$state
    if (is.applicable(state,action,problem)){
      newnode = list()
      newnode$parent  = node
      newnode$parent$parent = NULL
      newnode$state   = effect(state,action)    
      newnode$actions = rbind(node$actions,action)
      newnode$depth   = node$depth+1
      newnode$cost    = node$cost+get.cost(actions.possible[i,],state)
      newnode$evaluation = get.evaluation(newnode$state,problem)
      newnodes        = append(newnodes,list(newnode))
    }
  }
  return(newnodes)
}