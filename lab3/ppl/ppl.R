# Simulator of a packet switched point-to-point link, with 2 scheduling
# mechanisms (fifo and strict priority), and 2 source types (both with Poisson
# arrivals but one with exponentially distributed packet sizes and the other
# with fixed packet sizes). It estimates the average packet delay, and the
# throughput of each flow.

source("parameters.R")
source("init.R")
source("timing.R")
source("arrivals.R")
source("departures.R")
source("pq.R")
source("report.R")

parameters() #Definition of input parameters

init() #Initialization of data structures

#Main program
while (Time < endTime) {
  #print(numPacketsInQueues)
  nextEvent=timing() #Next event
  nextFlow=nextEvent[2] #Flow of next event
  nextEventType=nextEvent[3] #Type of next event
  if (nextEventType==1) { #If next event is an arrival
    arrivals(nextFlow) #Call arrivals routine
  } else { #If next event is a departure
    departures() #Call departures routine
  }
}

report() #Computes and prints the performance metrics