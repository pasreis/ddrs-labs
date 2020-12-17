source("lsroute_aux.R")
source("shortestpath.R")

numNodes=nrow(R) #Total number of nodes
numLinks=sum(R) #Total number of links

Mu=1e9/8e3 #Service rate in packets/second

TotalRate=sum(Tr)*1e6/8e3 #Total traffic rate in packets/second

####Solution A

#Prepares Costs matrix for ShortestPath function
Costs=L;Costs[R==0]=Inf #Cost is infinity if there is no link

Routes=list() #Initialize list that stores the shortest paths between each pair of nodes
Rates=matrix(rep(0,len=numNodes^2),nrow=numNodes) #Initialize matrix that stores the total traffic rates of each link

numRoute=1
for (i in 1:numNodes) {
  for (j in 1:numNodes) {
    if (i!=j) {
      thisRouteCost=ShortestPath(Costs,i,j) #Determines shortest path from i to j (and its cost) 
      thisRoute=thisRouteCost[-length(thisRouteCost)] #Extracts path from thisRouteCost vector
      Routes[[numRoute]]=thisRoute #Stores shortest path in Routes list
      lenRoute=length(thisRoute) #Determines length of route
      for (k in 1:lenRoute-1) { #Updates link rates
        o=thisRoute[k] #Origin of link
        d=thisRoute[k+1] #Destination of link
        Rates[o,d]=Rates[o,d]+Tr[i,j]*1e6/8e3 #Increments traffic rate of link from o to d
      }
      numRoute=numRoute+1
    }
  }
}

LinkLoads=Rates/Mu #Link loads
MaxLoad=max(LinkLoads) #Maximum of link loads
AverageLoad=sum(LinkLoads)/numLinks #Average of link loads
LinkDelays=R*(1/(Mu-Rates)+L*1e3/3e8) #Link average delays
LinkPackets=Rates*LinkDelays #Link average number of packets
AverageDelay=sum(LinkPackets)/TotalRate #Average packet delay

cat(sprintf("Maximum link load in scenario A = %f",MaxLoad),"\n")
cat(sprintf("Average packet delay in scenario A = %f",AverageDelay),"\n")

####Solution B

Rates=matrix(rep(0,len=numNodes^2),nrow=numNodes) #Initialize matrix that stores the total traffic rates of each link

numODPairs=numNodes^2-numNodes #Total number of OD pairs

#Builds list of remaining OD pairs, starting with all OD pairs 
RemainingODPairs=list()
k=1
for (i in 1:numNodes) {
  for (j in 1:numNodes) {
    if (i!=j) {
      RemainingODPairs[[k]]=c(i,j)
      k=k+1
    }
  }
}

#Assigns a path to each flow (OD pair), one by one
numRemainingODPairs=numODPairs #Number of remaining OD pairs
while (numRemainingODPairs>0) { #Loops until all OD pairs are considered
  LinkDelays=1/(Mu-Rates)+L*1e3/3e8 #Link average delays
  LinkDelays[R==0]=Inf #Link average delay is infinity if there is no link
  FlowDelays=c(length=numRemainingODPairs) #Initializes FlowDelays vector, which stores the average delay of each flow
  Routes=list() #Initializes Routes list, which stores the shortest path of each flow
  for (i in 1:numRemainingODPairs) {
    ODpair=RemainingODPairs[[i]] #Reads remaining OD pair
    RouteCost=ShortestPath(LinkDelays,ODpair[1],ODpair[2]) #Determines shortest path of OD pair (and its cost)
    FlowDelays[i]=RouteCost[length(RouteCost)] #Stores cost (delay) of shortest path
    Routes[[i]]=RouteCost[1:(length(RouteCost)-1)] #Stores route of shortest path
  }
  BestODpairIndex=which.min(FlowDelays) #Determines index of best flow (flow with minimum delay)
  BestODPair=RemainingODPairs[[BestODpairIndex]] #Determines OD pair of best flow
  RemainingODPairs[[BestODpairIndex]]=NULL #Removes OD pair from list of remaining OD pairs
  numRemainingODPairs=length(RemainingODPairs) #Updates number of remaining OD pairs
  BestRoute=Routes[[BestODpairIndex]] #Reads route of best flow
  lenRoute=length(BestRoute) #Length of route of best flow
  ODRate=Tr[BestODPair[1],BestODPair[2]]*1e6/8e3 #Reads traffic rate of best flow
  for (k in (1:lenRoute-1)) { #Installs traffic rate of best flow in each of its links
    o=BestRoute[k] #Origin of link
    d=BestRoute[k+1] #Destination of link
    Rates[o,d]=Rates[o,d]+ODRate #Increments traffic rate of link from o to d
  }
}

LinkLoads=Rates/Mu #Link loads
MaxLoad=max(LinkLoads) #Maximum of link loads
AverageLoad=sum(LinkLoads)/numLinks #Average of link loads
LinkDelays=R*(1/(Mu-Rates)+L*1e3/3e8) #Link average delays
LinkPackets=Rates*LinkDelays #Link average number of packets
AverageDelay=sum(LinkPackets)/TotalRate #Average packet delay

cat(sprintf("Maximum link load in scenario B = %f",MaxLoad),"\n")
cat(sprintf("Average packet delay in scenario B = %f",AverageDelay),"\n")