# Constants
N <- 100000

states <- c(1:3)

# Construct Lamba matrix
lambdas <- matrix(0, nrow=3, ncol=3)
lambdas[2,1] <- 1
lambdas[2,3] <- 1
lambdas[3,1] <- 1
lambdas[3,2] <- 1
lambdas[1,2] <- 2

# Construct P matrix
P <- matrix(1, nrow=3, ncol=3)
for (i in (1:3)) {
	for (j in (1:3)) {
		if (i == j) {
			P[i, j] <- 0
		} else {
			if (lambdas[i, j] == 0)  {
				P[i, j] <- 0
			} else {
				P[i, j] <- 1 / 2
			}
		}
	}
}

# print(P)
# print(lambdas)


# ------------------------------------- THEORETICAL --------------------------------------
# reduced system of equations to calculate theoretical limiting probabilities
# Pi1 + Pi2 + Pi3 = 1
# Pi1*Lambda12 - Pi2*Lambda21 - Pi3*Lambda31 = 0
#  - Pi2*Lambda23 + Pi3*(Lambda32 + Lambda31) = 0

# corrisponding indexes matrix for solving the system
a <- matrix(0, nrow=3, ncol=3);
a[1,1] = 1
a[1,2] = 1
a[1,3] = 1
a[2,1] = 2 # Lambda[1,2]
a[2,2] = -1 # -Lambda[2,1]
a[2,3] = -1 #-Lambda[3,1]
# a[3,1] = 0
a[3,2] = -1 # -Lambda[2,3]
a[3,3] = 2 # (Lambda32 + Lambda31)

b <- matrix(0, nrow= 3 ,ncol = 1)
b[1,1] = 1
# b[2,1] = 0
# b[3,1] = 0

# P is a 1x3 matrix (vector)
Pi<-solve(a,b)
print(paste("Theoretical Limiting Probabilities: ", Pi[1],Pi[2],Pi[3]))

# ---------------------------------------- VIEW 1 ----------------------------------------
# generate stationary time for every state, then transition based on the probability

# Define initial state at random
state <- 1
next_jump <- Inf

transactions <- matrix(0, nrow=3, ncol=3)
tot_time = rep(0,3)

for (i in (0:N)) {
	
	next_jump <- rexp(1, sum(lambdas[state,]))
	
	# 1/next_jump is the sojourn time (next_jump is the rate)
	tot_time[state] = tot_time[state] + next_jump
	# print(paste("Taking ", next_jump, " to change state"))
	# print(paste("Total for state ",state, " = ", tot_time[state]))

	if (state == 1) {
		state <- 2
		# print(state)
	} else if (state == 2) {
		state <- ifelse(next_jump < P[state,1] , 1, 3)
		# print(state)
	} else if (state == 3) {
		state <- ifelse(next_jump < P[state,2] , 2, 1)
		# print(state)
	}
}

tot_time = tot_time / sum(tot_time)
print("                              VIEW1                              ")
print(paste("Simulated Limiting Probabilities with N=", N, ": ",tot_time[1],tot_time[2],tot_time[3]))

# ---------------------------------------- VIEW 2 ----------------------------------------


#VIEW 2
# CALCULATE Ti and move accordingly to min(Ti)
state <- 1
next_jump <- Inf

T <- rep(0,3)
tot_time = rep(0,3)

for (i in (0:N)) {

	#calculate all Tjs
	for(j in (1:3))
	{
		T[j]=ifelse(lambdas[state,j] != 0, rexp(1, lambdas[state,j]), Inf)
	}
	
	#take the min
	tot_time[state] = tot_time[state] + min(T)
	#change the state accordingly
	state <- which.min(T);
}

tot_time = tot_time / sum(tot_time)
print("                              VIEW2                              ")
print(paste("Simulated Limiting Probabilities with N=", N, ": ",tot_time[1],tot_time[2],tot_time[3]))