ShortestPath = function (Costs,Source,Destination) {
  
  #Given a matrix of costs this function determines the shorthest path between a
  #source and destination using the Dijkstra's algorithm. The route is the
  #sequence of nodes from source to destination. Here is an example of a costs
  #matrix:
  #Costs = [0       10      10      Inf
  #         10      0       10      10
  #         20      5       0       10
  #         Inf     25      5       0];
  
  NumNodes=nrow(Costs); #Number of nodes
  
  NodeLabels=Costs[Source,] #Vector of node label
  
  TempNodes=1:NumNodes #Vector of temporary nodes
  TempNodes=TempNodes[-Source] #Remove source node from vector of temporary nodes
  TempLabels=NodeLabels #Vector of temporary labels
  TempLabels=TempLabels[-Source] #Remove label of source node from vector of temporary labels
  
  NodeParents=rep(Source,NumNodes) #Vector of node parents
  
  p=Source #Node that becomes permanently labelled
  
  while (TRUE) { 
    i=which.min(TempLabels) #Determines index of lower label node
    c=min(TempLabels) #Determines label of lower label node
    p=TempNodes[i] #p is the next permanent node
    TempNodes=TempNodes[-i] #Remove p node from vector of temporary nodes
    TempLabels=TempLabels[-i] #Remove label of p node from vector of temporary labels
    NodeLabels[p]=c #Update node label of p node
    if (p==Destination) break #Iteration ends when destination found
    for (k in 1:length(TempNodes)) { #Update labels of temporary nodes
      nt=TempNodes[k] #Next temporary node
      ct=NodeLabels[p]+Costs[p,nt] #Calculate cost to temporary node via p
      if (ct < NodeLabels[nt]) { #Update only if new cost is lower
        TempLabels[k]=ct #Update label on vector of temporary node labels
        NodeLabels[nt]=ct #Update label on vector of node labels
        NodeParents[nt]=p #Update node parent
      }
    }
  }
  
  #Retermines route from vector of node parents  
  Route=c(Destination) 
  r=Destination
  while (r!=Source) {
    r=NodeParents[r]
    Route=cbind(r,Route)
  }
  
  return(c(Route,c))
  
}