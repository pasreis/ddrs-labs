ArrivalRate=3 # lamda
ServiceRate=4 # mu
set.seed(62)
n = 1500 # number of clients that traversed the queue
Time=0
NumQueueCompleted=0
numSystemCompleted = 0
ServerStatus = 0
NumInQueue=0
AreaServerStatus=0
TimePreviousEvent=0
AcumDelay=0
AcumServiceTime <- 0
QueueArrivalTime=c()
EventList=c(rexp(1,ArrivalRate),Inf)
StartServiceTime <- 0
while (NumQueueCompleted<n) {
	NextEventType=which.min(EventList)
	Time=EventList[NextEventType]
	AreaServerStatus = AreaServerStatus + ServerStatus * (Time - TimePreviousEvent)
	if (NextEventType==1) {
		EventList[1]=Time+rexp(1,ArrivalRate)
		if (ServerStatus==1) {
			QueueArrivalTime=c(QueueArrivalTime,Time)
			NumInQueue=NumInQueue+1
		} else {
			NumQueueCompleted=NumQueueCompleted+1
			ServerStatus=1
			StartServiceTime <- Time
			EventList[2]=Time+rexp(1,ServiceRate)
		}
	} else {
		if (NumInQueue==0) {
			ServerStatus=0
			AcumServiceTime <- AcumServiceTime + (Time - StartServiceTime)
			EventList[2]=Inf
		} else {
			AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
			QueueArrivalTime=QueueArrivalTime[-1]
			NumInQueue=NumInQueue-1
			NumQueueCompleted=NumQueueCompleted+1
			EventList[2]=Time+rexp(1,ServiceRate)
		}
		numSystemCompleted = numSystemCompleted + 1
	}
	TimePreviousEvent = Time
}
AvgDelay=AcumDelay/NumQueueCompleted
ServerUtilization = AreaServerStatus / Time
AvgSystemDelay <- (AcumServiceTime + AcumDelay) / numSystemCompleted

TheoAvgDelay <- ArrivalRate / (ServiceRate * (ServiceRate - ArrivalRate))
TheoServiceDelay <- 1 / (ServiceRate - ArrivalRate)

print("Theoretical Results:")
print(paste("  Average Delay in Queue:", TheoAvgDelay))
print(paste("  Average System Delay:", TheoServiceDelay))

print("")
print(paste("Average Delay in Queue: ", AvgDelay))
print(paste("Average System Delay: ", AvgSystemDelay))
print(paste("Server Utilization: ", ServerUtilization * 100, "%"))