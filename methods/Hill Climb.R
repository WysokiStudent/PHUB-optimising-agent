Hill.Climb = function(problem,
                                count.limit=100, 
                                count.print = 100, 
                                trace = FALSE){
  
  name.method = "Hill Climb"
  state.initial = problem$state.initial
  state.final = problem$state.final
  actions.possible = problem$actions.possible
  
  node = list(parent=c(),
              state=state.initial,
              actions=c(),
              depth=0,
              evaluation = get.evaluation(state.initial, problem),
			        cost=0)
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
      print(paste0("(depth=",firstnode$depth,", cost=",firstnode$depth,")"),quote = F)
    }
    
    newnodes = expand.node(firstnode, actions.possible)
    
    nodes.added.frontier = 0
    if (length(newnodes)){
      newnode = newnodes[which.min(sapply(newnodes, function(x) x$evaluation))][[1]]
      if (firstnode$evaluation <= newnode$evaluation){
        end.reason = "Sollution"
        break
      }
			frontier = append(frontier,list(newnode))
			nodes.added.frontier = nodes.added.frontier + 1
      if (trace){
        print(paste0("State added to frontier: - (depth=",newnode$depth,", cost=",newnode$depth,")"),quote = F)
        to.string(newnode$state)
      }
    } else{
      end.reason = "Frontier"
      break
    }
    
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
    print("Solution found!", quote = F)
  } else{
    if (end.reason == "Frontier"){
      print("Empty frontier", quote = F)
    } else{
      print("Maximum Number of iterations reached", quote = F)
    }
  }
  to.string(firstnode$state)
  print("Evaluation is: ")
  print(firstnode$evaluation)
  print("Actions: ", quote = F)
  print(firstnode$actions, quote = F)
  result$state.final = firstnode
  
  plot.results(report,name.method,problem)
  
  return(result)
}
