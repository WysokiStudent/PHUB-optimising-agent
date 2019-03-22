plot.results = function(report,name.method,problem){
  # Generation of Graphics...
  plot1 = ggplot(report,aes(x=iteration,y=nodes.frontier))+geom_line(col="dodgerblue",size = 2)+
    labs(x="Iteration Number",
         y="Nodes stored in the frontier",
         title="Size of the Frontier",
         caption = "University of Deusto")+theme_minimal()
  
  plot2 = ggplot(report,aes(x=nodes.added.frontier))+geom_histogram(fill="dodgerblue",
                                                                    bins=length(unique(report$depth.of.expanded)),
                                                                    binwidth = 0.1)+
    labs(x="Number nodes added to frontier",
         y="Frequency",
         title="Frontier growth",
         caption = "University of Deusto")+theme_minimal()
  
  plot3 = ggplot(report,aes(x=depth.of.expanded))+geom_histogram(fill="dodgerblue",
                                                                 bins=length(unique(report$depth.of.expanded)),
                                                                 binwidth = 0.1)+
    labs(x="depth",
         y="Number of Nodes",
         title="depth of the nodes in the frontier",
         caption = "University of Deusto")+theme_minimal()
  
  grid.arrange(plot1,plot2,plot3,ncol=1,top=paste0(name.method,": ",problem$name))
}