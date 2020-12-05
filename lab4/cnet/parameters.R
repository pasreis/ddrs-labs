#This is the function where the user defines the simulation scenario

parameters = function() {
  
  #Links must be identified by contiguous integers starting at 1. These numbers
  #must be used as indexes of the LinkCapacities vector and of the vectors
  #defining the routes of each flow. Do not use the node numbers for this
  #purpose!
  
  #Define here the capacity of each link. The capacity may be expressed in
  #bits/sec or in number of circuits. If expressed in bits/sec, the call
  #bandwidth of each flow (bwd parameter of Flows list) must also be expressed in bits/s; if
  #expressed in number of circuits, the call bandwith is the number of circuits
  #ocuppied by each call
  LinkCapacities<<-c(0,5,13,0,13,7,0,13,0,0)
  
  #Define here the flows. Flows is a list of lists that stores in each list the
  #call duration, call arrival rate, call bandwidth and the route of each flow;
  #the routes must be defined using the link identifiers (and not the node
  #identifiers) bwds: 20,13,13,13,18,20
  Flows<<-list(list(duration=0.2,rate=5,bwd=1,route=c(5,6)),
               list(duration=1,rate=5,bwd=1,route=c(5)),
               list(duration=1,rate=5,bwd=1,route=c(8)),
               list(duration=1,rate=5,bwd=1,route=c(3)),
               list(duration=0.2,rate=5,bwd=1,route=c(2,3)),
               list(duration=0.2,rate=5,bwd=1,route=c(8,6)))
  
  #Definition of the simulation end time, function of the minimum arrival rate
  endTime<<-10000*(1/5)
  
}