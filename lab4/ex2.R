# CONSTANTS
ARRIVAL1 = 1
ARRIVAL2 = 2
TRANSMISSION = 3

# PARAMETERS
arrivalRate = c(4,9)
avgPacketSize = c(500, 170)
quantum = c(1500,900)
n = 1000
capacity=4000

# VARIABLES
EventList <- rep(Inf, 3)
packets = 0
activeQueue = 1
first = c()
second = c()
transChannel = c()
queue = list(first,second,transChannel)
time = 0

# STATISTICAL COUNTERS
deficit1 = 0
deficit2 = 0

EventList[1] = rexp(1,arrivalRate[1])
EventList[2] = rexp(1,arrivalRate[2])
packets = packets + 2

while(packets < n){
    nextEvent = which.min(EventList)

    if(nextEvent == ARRIVAL1){
        list[ARRIVAL1] = c(list[ARRIVAL1],min(EventList))
        EventList[1] = rexp(1,arrivalRate[1])

    } else if (nextEvent == ARRIVAL2){
        list[ARRIVAL2] = c(list[ARRIVAL2],min(EventList))
        EventList[2] = rexp(1,arrivalRate[2])
        

    } else { # TRANSMISSION FROM LINK
        
        if(activeQueue == 1){
            deficit1 = deficit1 + quantum1
            nextPacket = runif(1, avgSize1*2)
            while(<=deficit1){
                deficit1 = deficit1 - nextPacket
                print(paste("1 - ", nextPacket))
            }
        }

    }
    

}