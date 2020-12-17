BuildSolution=function() {
  #Function that builds a solution using a greedy randomized strategy
  #Requires as global numODPairs, numNodes, numRoutes, R, Tr, L, Mu
  
  #Builds list of OD pairs ordered randomly
  ODPairs=list()
  a=sample(1:numODPairs) #Vector of indexes ordered randomly
  k=1
  for (i in 1:numNodes) {
    for (j in 1:numNodes) {
      if (i!=j) {
        ODPairs[[a[k]]]=c(i,j)
        k=k+1
      }
    }
  }
  Rates=matrix(rep(0,len=numNodes^2),nrow=numNodes) #Initializes matrix that stores the rate of each link
  Routes=list()
  for (i in 1:numRoutes){
    LinkDelays=1/(Mu-Rates)+L*1e3/3e8 #Link average delays
    LinkDelays[R==0]=Inf #Link average delay is infinity if there is no link
    thisODPair=ODPairs[[i]]
    ODPairOrigin=thisODPair[1]
    ODPairDestination=thisODPair[2]
    RouteCost=ShortestPath(LinkDelays,ODPairOrigin,ODPairDestination)
    lenRoute=length(RouteCost)-1
    thisRoute=RouteCost[1:lenRoute]
    ODRate=Tr[ODPairOrigin,ODPairDestination]*1e6/8e3 #Reads traffic rate of best flow
    for (k in 1:(lenRoute-1)) { #Installs rate of best flow in each of its links
      o=thisRoute[k] #Origin of link
      d=thisRoute[k+1] #Destination of link
      Rates[o,d]=Rates[o,d]+ODRate #Increments rate of link from o to d
    }
    Routes[[i]]=thisRoute
  }
 return(Routes)
}



