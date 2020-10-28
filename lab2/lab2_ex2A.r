ArrivalRate=1 # lambda
ServiceRate=2 # mu
Time=0
NumQueueCompleted=0
ServerStatus=c(0,0)
NumInQueue=0
AcumDelay=0
QueueArrivalTime=c()
EventList=c(rexp(1,ArrivalRate),Inf, Inf)
while (NumQueueCompleted<1000) {
	NextEventType=which.min(EventList)
	Time=EventList[NextEventType]
	if (NextEventType==1) {
		EventList[1]=Time+rexp(1,ArrivalRate)
		availableServer <- match(0, ServerStatus)
		if (is.na(availableServer)) {
			# There is not any server available
			QueueArrivalTime=c(QueueArrivalTime,Time)
			NumInQueue=NumInQueue+1
		} else {
			# There is at least 1 server available
			NumQueueCompleted=NumQueueCompleted+1
			ServerStatus[availableServer]=1
			EventList[availableServer + 1]=Time+rexp(1,ServiceRate) # +1 becuase position 0 is for arrivals
		}
	} else {
		if (NumInQueue==0) {
			busyServer <- NextEventType - 1 # -1 because position 0 is for arrivals
			ServerStatus[busyServer]=0
			EventList[NextEventType]=Inf
		} else {
			AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
			QueueArrivalTime=QueueArrivalTime[-1]
			NumInQueue=NumInQueue-1
			NumQueueCompleted=NumQueueCompleted+1
			EventList[NextEventType]=Time+rexp(1,ServiceRate)
		}
	}
}
AvgDelay=AcumDelay/NumQueueCompleted
rho = ArrivalRate / (2 * ServiceRate)
p_q = ((2 * rho) ^ (2)) / (2 * (1 - rho) * ((1 + (2 * rho)) + (((2 * rho) ^ 2) / (2 * (1 - rho)))))
TheoAvgDelay = p_q * (rho / (ArrivalRate * (1 - rho)))
print(paste("Theoretical average delay in queue:", TheoAvgDelay))
print(paste("Average Delay in queue: ", AvgDelay))