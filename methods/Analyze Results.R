analyze.results = function(all,problem){
  results = data.frame()
  for (i in 1:length(all)){
    name = all[[i]]$name
    print(name)
    solution.found = any(!is.na(all[[i]]$state.final))
    if (any(solution.found)){
      # Checking the solution
      solution.length = length(all[[i]]$state.final$actions)
      solution.cost   = all[[i]]$state.final$cost
      print("  Solution Found! :)",quote = F)
      actions = all[[i]]$state.final$actions
      state.current = problem$state.initial
      print("Initial State: ")
      to.string(state.current)
      for (a in 1:length(actions)){
        action = actions[a]
        state.current = effect(state.current,action)  
        print(paste0("After applying action: ",action),quote = F)
        to.string(state.current)
      }
    }else{
      solution.length = -1
      solution.cost   = -1
      print("  No Solution Found :(")
    }
    
    number.expanded = length(all[[i]]$report$iteration)
    
    maximum.depth = max(all[[i]]$report$depth.of.expanded)
    
    maximum.frontier = max(all[[i]]$report$nodes.frontier)
    
    results = rbind(results,data.frame(name=name,
                                       solution=solution.found,
                                       length=solution.length,
                                       cost=solution.cost,
                                       expanded=number.expanded,
                                       max.depth=maximum.depth,
                                       max.frontier=maximum.frontier))
    
  }
  return(results)
}