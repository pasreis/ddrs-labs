source("Z:\\Documents\\MEIC\\SEMESTRE3\\DDRS\\labs\\lab5\\BuildSolution.R")
source("Z:\\Documents\\MEIC\\SEMESTRE3\\DDRS\\labs\\lab5\\localsearchrouting\\lsroute_aux.R")
source("Z:\\Documents\\MEIC\\SEMESTRE3\\DDRS\\labs\\lab5\\localsearchrouting\\shortestpath.R")
# source("Enrico metti qua il tuo path per il file BuildSolution")
# source("path per localsearchrouting/lsroute_aux.R")

numNodes <<- nrow(R)
numODPairs <<- numNodes^2 - numNodes #Total number of OD pairs
numRoutes <<- numODPairs
Mu <- 1e9
Rates <- matrix(rep(0, len=numNodes^2), nrow=numNodes)

UpdateRates <- function(solution) {
	for (route in solution) {
		lenRoute <- length(route)
		for (i in 1:numNodes) {
			for (j in 1:numNodes) {
				for (k in 1:(lenRoute - 1)) {
					src <- route[k]
					dst <- route[k + 1]
					Rates[src,dst] <<- Rates[src,dst] + Tr[i,j]
				}
			}
		}
	}
}

Evaluate <- function(solution) {
	#Computes the maximum link load and the average packet delay of a solution, using the Kleinrock approximation.
	#print("Evaluate: Started!")
	#pdateRates(solution)
	
	for (route in solution) {
		lenRoute <- length(route)
		for (i in 1:numNodes) {
			for (j in 1:numNodes) {
				for (k in 1:(lenRoute - 1)) {
					src <- route[k]
					dst <- route[k + 1]
					Rates[src,dst] <<- Rates[src,dst] + Tr[i,j]
				}
			}
		}
	}
	
	TotalRate=sum(Tr)*1e6/8e3 #Total traffic rate in packets/second
	LinkDelays=R*(1/(Mu-Rates)+L*1e3/3e8) #Link average delays
	LinkPackets=Rates*LinkDelays #Link average number of packets
	AverageDelay=sum(LinkPackets)/TotalRate #Average packet delay

	#print(paste("Evaluate: average delay =", AverageDelay))
	AverageDelay
}

BuildNeighbour <- function(solution, i) {
	#print("TODO BuildNeighbour function")
	# Remove flow i
	removed_flow <- solution[[i]]
	solution[[i]] <- NULL

	# Update Rates
	#UpdateRates(solution)

	for (route in solution) {
		lenRoute <- length(route)
		for (i in 1:numNodes) {
			for (j in 1:numNodes) {
				for (k in 1:(lenRoute - 1)) {
					src <- route[k]
					dst <- route[k + 1]
					Rates[src,dst] <<- Rates[src,dst] + Tr[i,j]
				}
			}
		}
	}
	
	# Apply Dijstra
	src <- removed_flow[1]
	dst <- removed_flow[length(removed_flow)]
	other_route <- ShortestPath(Rates, src, dst)
	other_route <- other_route[-length(other_route)]
	# Insert in Routes
	solution[[length(solution) + 1]] <- other_route

	#print("BuildNeighbour: solution =")
	# Return Routes
	solution
}

GlobalBest <- Inf
numIterations <- 2

for (n in 1:numIterations) {
	print(paste("iteration:",n))
	Rates <- matrix(rep(0, len=numNodes^2), nrow=numNodes)
	CurrentSolution <- BuildSolution()
	CurrentObjective <- Evaluate(CurrentSolution)
	print(paste("Current Average Delay:", CurrentObjective))
	rep <- TRUE

	while (rep == TRUE) {
		NeighbourBest <- Inf
		NeighbourBestSolution <- Inf # not sure about this
		
		for (i in 1:numRoutes) {
			NeighbourSolution <- BuildNeighbour(CurrentSolution, i)
			NeighbourObjective <- Evaluate(NeighbourSolution)
			if (NeighbourObjective < NeighbourBest) {
				NeighbourBest <- NeighbourObjective
				NeighbourBestSolution <- NeighbourSolution
			}
		}

		print("=================================================================")
		print(paste("NeighbourBest =", NeighbourBest))
		print(paste("CurrentObjective =", CurrentObjective))
		if (NeighbourBest < CurrentObjective) {
			print("NOK, other cylce")
			CurrentObjective <- NeighbourBest
			CurrentSolution <- NeighbourBestSolution
		} else {
			print("OK, other iteration")
			rep <- FALSE
		}
		print("=================================================================")
	}

	if (CurrentObjective < GlobalBest) {
		GlobalBestSolution <- CurrentSolution
		GlobalBest <- CurrentObjective
	}
}

print(Rates)
LinkLoads <- Rates / Mu
print(paste("Maximum Link Load:", max(LinkLoads)))
print(paste("Average Packet Delay:", GlobalBest))







