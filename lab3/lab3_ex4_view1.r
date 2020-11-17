# Constants
N <- 1000

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

print(P)
print(lambdas)

# Define initial state at random
state <- round(runif(1, min=1, max=3), digits=0)
next_jump <- Inf

#VIEW 1
# generate vi for every state, then transition based on the probability

transactions <- matrix(0, nrow=3, ncol=3)
tot_time = rep(0,3)
previous_jump = 0

for (i in (0:N)) {
	next_jump <- rexp(1, sum(lambdas[state,]))
	
	# 1/next_jump is the sojourn time (next_jump is the rate)
	tot_time[state] = tot_time[state] + next_jump - previous_jump
	previous_jump = next_jump

	if (state == 1) {
		state <- 2

	} else if (state == 2) {

		state <- ifelse(next_jump < P[state,1] , 1, 3)

	} else if (state == 3) {

		state <- ifelse(next_jump < P[state,2] , 2, 1)
	}
}

tot_time = tot_time / sum(tot_time)
print(tot_time)
