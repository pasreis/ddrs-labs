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
state <- 1
next_jump <- Inf

#VIEW 2
# CALCULATE Ti and move accordingly to min(Ti)
tot_time = rep(0,3)

T <- rep(0,3)

for (i in (0:N)) {

	for(j in (1:3))
	{
		T[j]=ifelse(lambdas[state,j] != 0, rexp(1, lambda[i,j]), Inf)
	}
	
	tot_time[state] = tot_time[state] + min(T)
	state <- which.min(T);
}