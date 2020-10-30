 
simulate = function(k, ArrivalRate, ServiceRate, N) {
	Time=0
	NumQueueCompleted=0
	ServerStatus=0
	NumInQueue=0
	AcumDelay=0
	QueueArrivalTime=c()
	EventList=c(rexp(1,ArrivalRate),Inf)
	while (NumQueueCompleted<N) { 
		NextEventType=which.min(EventList)
		Time=EventList[NextEventType]
		if (NextEventType==1) {
			EventList[1]=Time+rexp(1,ArrivalRate)
			if (ServerStatus==1) {
				QueueArrivalTime=c(QueueArrivalTime,Time)
				NumInQueue=NumInQueue+1
			} else {
				NumQueueCompleted=NumQueueCompleted+1
				ServerStatus=1
				EventList[2]=Time+rexp(1,ServiceRate)
			}
		} else {
			if (NumInQueue==0) {
				ServerStatus=0
				EventList[2]=Inf
			} else {
				if (NumQueueCompleted > k) {
					AcumDelay=AcumDelay+Time-QueueArrivalTime[1]	
				}
				QueueArrivalTime=QueueArrivalTime[-1]
				NumInQueue=NumInQueue-1
				NumQueueCompleted=NumQueueCompleted+1
				EventList[2]=Time+rexp(1,ServiceRate)
			}
		}
	}
	AvgDelay=AcumDelay/ (NumQueueCompleted - k)
}