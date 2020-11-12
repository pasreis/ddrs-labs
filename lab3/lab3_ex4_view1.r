# Constants
N <- 1000

states <- c(1:3)

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
# Define initial state
state <- round(runif(1, min=1, max=3), digits=0)
next_jump <- Inf

for (k in (0:N)) {
	next_jump <- dexp(1, sum(lambdas[state,]))
	if (state == 1) {
		state <- 1
	} else if (state == 2) {
		next_jump <- dexp(1, sum(lambdas[2,]))
		state <- ifelse(next_jump > 0.5, 1, 3)
	} else if (state == 3) {
		next_jump <- dexp(1, sum(lambdas[3,]))
		state <- ifelse(next_jump > 0.5, 2, 1)
	}
}