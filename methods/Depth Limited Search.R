Depth.Limited.Search = function(problem,
                              count.limit=100, 
                              count.print = 100, 
                              trace = FALSE, 
                              graph.search = FALSE,
                              depth.limit = 10){
  
  name.method = paste0("Depth Limited Search ",depth.limit, ifelse(graph.search," + GS",""))
  state.initial    = problem$state.initial
  state.final      = problem$state.final
  actions.possible = problem$actions.possible
  
  node = list(parent=c(),
              state=state.initial,
              actions=c(),
              depth=0,
              cost=0)
  frontier = list(node)
  
  if (graph.search){
    expanded = list()     
  }
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
    # We only change here with regards to BFS
    firstnode = frontier[[length(frontier)]]
    frontier[[length(frontier)]] = NULL
    if(graph.search){
      expanded = append(expanded,list(firstnode))
    }
    
    if (trace){
      print(" ",quote = F)
      print("------------------------------", quote = F)
      print("State extracted from frontier:", quote = F)
      to.string(firstnode$state)
      print(paste0("(depth=",firstnode$depth,", cost=",firstnode$depth,")"),quote = F)
    }
    
    if (is.final.state(firstnode$state,state.final)){
      end.reason = "Sollution"
      break
    }
    
    newnodes = expand.node(firstnode, actions.possible)
    
    if (!graph.search){
      # frontier = c(frontier,newnodes)
      nodes.added.frontier = 0
      if (length(newnodes)){
        for (i in 1:length(newnodes)){
          newnode = newnodes[[i]]		  
          if (newnode$depth <= depth.limit){
            frontier = c(frontier,newnodes)
            nodes.added.frontier = nodes.added.frontier + 1
            if (trace){
              print(paste0("State added to frontier: - (depth=",newnode$depth,", cost=",newnode$depth,")"),quote = F)
              to.string(newnode$state)
            }
          }
        }
      }
    }else{
      nodes.added.frontier = 0
      if (length(newnodes)){
        for (i in 1:length(newnodes)){
          newnode = newnodes[[i]]
          if (newnode$depth <= depth.limit){
            if (!any(sapply(frontier,function (x) identical(x$state,newnode$state)))){
              if (!any(sapply(expanded,function (x) identical(x$state,newnode$state)))){
                frontier = append(frontier,list(newnode))
                nodes.added.frontier = nodes.added.frontier + 1
                if (trace){
                  print(paste0("State added to frontier: - (depth=",newnode$depth,", cost=",newnode$depth,")"),quote = F)
                  to.string(newnode$state)
                }
              }
            }
          }
        }
      }
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
