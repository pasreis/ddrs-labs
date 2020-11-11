# constants
THINKING <- 0
BACKLOGGED <- 1

sigma <- 0.01
p <- 0.4
n <- 10

states <- c(0:n)

P <- matrix(0, nrow=n+1, ncol=n+1)

for (i in (0:n)) {
	for (j in (0:n)) {
		if (j < i - 1) {
			P[i+1, j+1] <- 0
		} else if (j == i - 1) {
			P[i+1, j+1] <- i * p * (1 - p) ^ (i - 1) * (1 - sigma)^(n - i)
		} else if (j == i) {
			P[i+1, j+1] <- (1 - sigma) ^ (n - i) * (1 - i * p * (1 - p) ^ (i - 1)) + (n - i) * sigma * (1 - sigma) ^ (n - i - 1) * (1 - p) ^ i
		} else if (j == i + 1) {
			P[i+1, j+1] <- (n - i) * sigma * (1 - sigma) ^ (n - i - 1) * (1 - (1 - p) ^ i)
		} else if (j > i + 1) {
			P[i+1, j+1] <- dim(combn(n - i, j - i))[2] * sigma ^ (j - i) * (1 - sigma) ^ (n - j)
		}
	}
}

state <- 0
num_suc_trans <- 0

E <- matrix(1, nrow=n+1, ncol=n+1)
I <- diag(n + 1)
vec <- rep(1, n + 1)
inverse <- solve(P + E - I)
Pi <- c()
for (i in (0:n+1)) { # even starting from 0 index start from 1
	Pi <- c(Pi, sum(inverse[,i]))
}

theo_throughput <- 0
for (i in (0:n)) {
	p_suc <- (n - i) * sigma * (1 - sigma) ^ (n - i - 1) * (1 - p) ^ (i) + i * p * (1 - p) ^ (i - 1) * (1 - sigma) ^ (n - i)
	theo_throughput <- theo_throughput + (p_suc * Pi[i + 1])
}

print(paste("Theoretical throughput", theo_throughput))

t_total <- 1000
clients <- rep(THINKING, n)
transmissions <- rep(0, n)
channel_use <- 0

for (i in (0:t_total)) {
	for (j in (1:n)) {
		if (clients[j] == THINKING) {
			transmissions[j] <- rbinom(1, 1, prob=p)	
		} else { # clients[j] == BACKLOGGED
			transmissions[j] <- rbinom(1, 1, prob=sigma)
		}
	}
	
	num_trans <- sum(transmissions == 1)

	if (num_trans == 1) { # No collisions
		channel_use <- channel_use + 1
		j <- match(1, transmissions)
		client[j] <- THINKING
	} else if (num_trans == 0) { # no transmission

	} else { #  transmission collided
		channel_use <- channel_use + 1
		for (j in (1:n)) {
			if (transmissions[j] == 1) {
				clients[j] <- BACKLOGGED
			}
		}
	}
}

throughput <- channel_use / t_total
print(paste("Simulated Throughput =", throughput))