#This program simulates a packet-switched network with fixed routing to estimate
#the average packet delay of each flow
source("/Users/enrico/Desktop/DDRS\ -\ Performance/LABS/ddrs-labs/lab4/pnet/parameters.R")
source("/Users/enrico/Desktop/DDRS\ -\ Performance/LABS/ddrs-labs/lab4/pnet/init.R")
source("/Users/enrico/Desktop/DDRS\ -\ Performance/LABS/ddrs-labs/lab4/pnet/timing.R")
source("/Users/enrico/Desktop/DDRS\ -\ Performance/LABS/ddrs-labs/lab4/pnet/arrivals.R")
source("/Users/enrico/Desktop/DDRS\ -\ Performance/LABS/ddrs-labs/lab4/pnet/departures.R")
source("/Users/enrico/Desktop/DDRS\ -\ Performance/LABS/ddrs-labs/lab4/pnet/report.R")

parameters()

init()

while (Time < endTime){
  nextEvent=timing() #Advances simulation clock and returns next event
  nextEventType=nextEvent[2] #Type of next event
  nextResourceNumber=nextEvent[3] #Resource number of next event
  if (nextEventType==1) {#If next event is an arrival at flow
    arrivals(nextResourceNumber) #Call arrival routine at specific flow
  }
  else { #If next event is a departure from a link
    departures(nextResourceNumber) #Call departure routine at specific link
  }
}

report()