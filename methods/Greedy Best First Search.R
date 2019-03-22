Greedy.Best.First.Search = function(problem,
                               count.limit=100, 
                               count.print = 100, 
                               trace = FALSE){
  
  name.method = "Greedy Best-First Search"
  state.initial    = problem$state.initial
  state.final      = problem$state.final
  actions.possible = problem$actions.possible
  
  node = list(parent=c(),
              state=state.initial,
              actions=c(),
              depth=0,
              cost=0,
              evaluation=0)
  frontier = list(node)
  
  count = 1
  end.reason = 0
  report = data.frame(iteration=numeric(),
                      nodes.frontier=numeric(),
                      depth.of.expanded=numeric(),
                      nodes.added.frontier=numeric())
  
  
  while (count<=count.limit){
    
    if (count%%count.print==0){
      print(paste0("Count: ",count,", Nodes in the frontier: ",length(frontier)), quote = F)
    }
    
    if (length(frontier)==0){
      end.reason = "Frontier"
      break
    }
    
    firstnode = frontier[[1]]
    frontier[[1]] = NULL
    
    if (trace){
      print(" ",quote = F)
      print("------------------------------", quote = F)
      print("State extracted from frontier:", quote = F)
      to.string(firstnode$state)
      print(paste0("(depth=",firstnode$depth,", cost=",firstnode$depth,", eval=",firstnode$evaluation,")"),quote = F)
    }
    
    if (is.final.state(firstnode$state,state.final)){
      end.reason = "Sollution"
      break
    }
    
    newnodes = expand.node(firstnode, actions.possible)
    
    frontier = c(frontier,newnodes)
    nodes.added.frontier = length(newnodes)
    
    # Line added to order the frontier according with EVALUATION
    frontier = frontier[order(sapply(frontier,function (x) x$evaluation))]
    
    if(trace){
      print(paste0("Total states in the frontier: ", length(frontier)),quote = F)
    }
    
    report = rbind(report,
                   data.frame(iteration = count,
                              nodes.frontier = length(frontier),
                              depth.of.expanded = firstnode$depth,
                              nodes.added.frontier = nodes.added.frontier))
    
    count = count+1
  }
  
  
  result = list()
  result$report = report
  result$name = name.method
  
  # Show the obtained (or not) final solution
  if (end.reason == "Sollution"){
    print("Solution found!!", quote = F)
    to.string(firstnode$state)
    print("Actions: ", quote = F)
    print(firstnode$actions, quote = F)
    result$state.final = firstnode
  } else{
    if (end.reason == "Frontier"){
      print("Empty frontier. No sollution found", quote = F)
    } else{
      print("Maximum Number of iterations reached. No sollution found", quote = F)
    }
    result$state.final = NA
  }
  
  plot.results(report,name.method,problem)
  
  return(result)
}
