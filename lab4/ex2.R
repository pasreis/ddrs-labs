# CONSTANTS

# PARAMETERS
arrivalRate = c(100,1)
avgPacketSize = c(10, 10)
quantum = c(50,50)
n = 1000
capacity=30000

# VARIABLES
EventList <- c(rexp(1,arrivalRate[1]),rexp(1,arrivalRate[2]), Inf)
packets = 2
activeQueue = 1
queue = list(c(),c())
queueCount = rep(0,2)
transmissionOngoing = FALSE
time = 0

# STATISTICAL COUNTERS
deficit = c(quantum[1],0)
totTransmitted = rep(0,2)

switchQueue = function() {
    activeQueue <<- ifelse(activeQueue == 1, 2, 1)
    if(length(queue[[activeQueue]]) > 0 ) {
        deficit[activeQueue] <<- deficit[activeQueue] + quantum[activeQueue]
        print(paste("switch to queue ", activeQueue, " new deficit: ",deficit[activeQueue], 
    " first packet size: ", queue[[activeQueue]][1]))
    } else {
        deficit[activeQueue] <<- 0
        print(paste("switch to queue ", activeQueue, " new deficit: 0, no packets in the queue"))
    }

}

transmitPacket = function() {
    transmissionOngoing <<- TRUE;
    print(paste("Transmission from queue ", activeQueue, 
    " Deficit= ", deficit[activeQueue]," first packet= ",queue[[activeQueue]][1]))

    deficit[activeQueue] <<- deficit[activeQueue] - queue[[activeQueue]][1]
    # print(paste("New Deficit = ",deficit[activeQueue]))

    EventList[3] <<- time + queue[[activeQueue]][1] / capacity 
    queue[[activeQueue]] <<- queue[[activeQueue]][-1]
    queueCount[activeQueue] <<- queueCount[activeQueue] -1
    totTransmitted[activeQueue] <<- totTransmitted[activeQueue] +1
    
}


while(packets < n){

    nextEvent = which.min(EventList)
    print(EventList)
    print(paste("Next event = ", nextEvent))
    time = min(EventList)

    if(nextEvent != 3){
        
        size = runif(1,min = 0, max = 2*avgPacketSize[nextEvent]) #calculate random size
        EventList[nextEvent] = time + rexp(1,arrivalRate[nextEvent])
        packets = packets+1

        if(queueCount[nextEvent] == 0 && activeQueue == nextEvent 
            && transmissionOngoing == FALSE && deficit[nextEvent] >= size){
            # here I don't touch any queue
            EventList[3] = size / capacity +time
            transmissionOngoing = TRUE
            totTransmitted[activeQueue] = totTransmitted[activeQueue] +1
            deficit[activeQueue] = deficit[activeQueue] - size
            print(paste(size, " arrives in queue ", activeQueue, ", empty link and enough deficit: GO"))
            #print(paste("It will take ",EventList[3])) #-time

        } else if(queueCount[nextEvent] == 0 && activeQueue == nextEvent 
                    && transmissionOngoing == FALSE && deficit[nextEvent]<size){
            # if the reason of the client going in the queue is JUST that the channel's deficit is not enough
            # then I switch queue until I don't have enough transmission deficit
            queue[[nextEvent]] = c(queue[[nextEvent]],size)
            queueCount[nextEvent] = queueCount[nextEvent] + 1

            # the && in the while condition server a purpose:
            # suppose a queue has not enough deficit and the second queue is empty
            # the switch would set to zero the second queue's deficit (bacause it's empty)
            # and the condition could make the loop terminate 
            # (extracting the first element of an empty vector is dubious)
            # by adding this condition we're covered from any shenanigan
            # as the loop always terminates on the non empty queue when it has enough deficit
            print(paste(size," arrives in queue ", activeQueue, ", empty link but not enough deficit:"))

           # I keep switching and adding until I have enough deficit in one of the queues
           # as long as at least one queue has some packets waiting
            nextSize = ifelse(queueCount[activeQueue]>0, queue[[activeQueue]][1], Inf)

            while(deficit[activeQueue] < nextSize && (queueCount[1]>0 || queueCount[2]>0)){
                switchQueue()
                # putting queue[[activeQueue]][1] in the condition makes it explode if the queue is empty
                # so I put Inf meaning that the control will switch again due to true condition
                # until the original queue has enough deficit

                nextSize = ifelse(queueCount[activeQueue]>0, queue[[activeQueue]][1], Inf)
            }

            # maybe now they're both 0 so I have to check
            if(queueCount[activeQueue]!=0)
                transmitPacket()
            else{
                print("No more packets waiting, idle channel")
                transmissionOngoing=FALSE
            }
            
        }
        else if (queueCount[activeQueue]==0 && nextEvent != activeQueue && transmissionOngoing==FALSE){
            #if a client arrives on the non active queue when the active queue is empty 
            #And the channel is free, I can switch and serve it immediately(OPTIMIZATION)
            print(paste(size, " arrives in queue ", nextEvent, ", the active queue is empty [",queueCount[activeQueue],
            " and the channel is also empty"))
            
            deficit[activeQueue] = 0
            activeQueue <<- ifelse(activeQueue == 1, 2, 1)
            deficit[activeQueue] = deficit[activeQueue] + quantum[activeQueue]
            print(paste("So I'm switching to queue ",activeQueue,"with new deficit: ",deficit[activeQueue]))
            
            #particular case when I know that the other queue is empty
            #so every time I switch twice to go back to the interested queue
            while(deficit[activeQueue] < size){
                activeQueue <<- ifelse(activeQueue == 1, 2, 1)
                deficit[activeQueue] = deficit[activeQueue] + quantum[activeQueue]
                activeQueue <<- ifelse(activeQueue == 1, 2, 1)
                deficit[activeQueue] = deficit[activeQueue] + quantum[activeQueue]
                print(paste("Not enough deficit, new deficit: ",deficit[activeQueue]))
            }
            EventList[3] = time + size/capacity
            transmissionOngoing = TRUE
            totTransmitted[activeQueue] = totTransmitted[activeQueue] +1
            print(paste("Now the packet is ready to be transmitted"))
        }
        else { # any other case put in the queue
            # other cases include: transmission already ongoing, other packets already in queue
            # or arrival in a non active queue but with transmission ongoing
            print(paste(size, " arrives in queue ", nextEvent, 
            " QUEUED because activeQueue=",activeQueue, " , transmission: ",transmissionOngoing,
            " remaining deficit: ",deficit[nextEvent]))
            queue[[nextEvent]] = c(queue[[nextEvent]],size)
            queueCount[nextEvent] = queueCount[nextEvent] + 1
            print(paste("There are ",queueCount[nextEvent], " clients in queue", nextEvent))
        }
    } else { # TRANSMISSION FROM LINK

        if (queueCount[activeQueue]>0 && deficit[activeQueue]>=queue[[activeQueue]][1]){
            print(paste("Other packet on queue ",activeQueue, " ready"))
            # other packets ready to be transmitted from the same queue
            transmitPacket()

        } else if (queueCount[activeQueue]>0 && deficit[activeQueue] < queue[[activeQueue]][1]){
            print(paste("other packets ready but deficit is inferior to the first packet size (",
            deficit[activeQueue],"<",queue[[activeQueue]][1],")"))
            
            #print(paste("Queue", activeQueue, ": ",queueCount[1]))
            #print(queue[[activeQueue]])
            nextSize = ifelse(queueCount[activeQueue]>0, queue[[activeQueue]][1], Inf)

            while(deficit[activeQueue] < nextSize && (queueCount[1]>0 || queueCount[2]>0)){
                switchQueue()
                # putting queue[[activeQueue]][1] in the condition makes it explode if the queue is empty
                # so I put Inf meaning that the control will switch again due to true condition
                # until the original queue has enough deficit

                nextSize = ifelse(queueCount[activeQueue]>0, queue[[activeQueue]][1], Inf)
            }

            if(queueCount[activeQueue]!=0)
                transmitPacket()
            else
            {
                transmissionOngoing=FALSE
                print("Not transmitting, no clients in queue")
            }
                

        } else { #if(queueCount[activeQueue] == 0) {
            print(paste("No more packets in queue ", activeQueue, 
            ": set deficit=0 and try the other one"))
            deficit[activeQueue] = 0
            switchQueue()

            if(queueCount[activeQueue] == 0){ # no more packets in the other queue as well
                print("Not transmitting, no clients in any queue")
                deficit[activeQueue] = 0
                transmissionOngoing = FALSE
                EventList[3] = Inf

            } else { # in case the other queue has some clients
                print(paste("There are ",queueCount[activeQueue], " packets waiting here"))
                if(deficit[activeQueue] >= queue[[activeQueue]][1]) {
                    transmitPacket()
                } else {
                    print("But not enough deficit")
                    # no reason to switch every time because the other queue is empty in this case
                    while(deficit[activeQueue] <= queue[[activeQueue]][1]){
                        deficit[activeQueue] = deficit[activeQueue] + quantum[activeQueue]
                    }
                    print(paste("Increased the deficit of ",activeQueue, " to ",deficit[activeQueue],
                    " to transmit the first packet because the other queue is empty"))
                    transmitPacket()
                }
            }
        }
    }
}

print(paste("Packets in Queue 1:",queueCount[1]))
print(paste("Packets in Queue 2:",queueCount[2]))
print(paste("Tot transmitted from Queue 1:",totTransmitted[1]))
print(paste("Tot transmitted from Queue 2:",totTransmitted[2]))
print(paste("Throughput of Queue 1:",totTransmitted[1]/sum(totTransmitted)))
print(paste("Throughput of Queue 2:",totTransmitted[2]/sum(totTransmitted)))
