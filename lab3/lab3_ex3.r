# constants
THINKING <- 0
BACKLOGGED <- 1

sigma <- 0.1
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

total_slots <- 10000
clients <- rep(THINKING, n)
transmissions <- rep(0, n)
num_suc_trans <- 0
num_thinking <- n
num_backlog <- 0

for (i in (0:total_slots)) {
	num_trans_thinking <- rbinom(1, num_thinking, prob=sigma)
	num_trans_backlog <- rbinom(1, num_backlog, prob=p)
	
	
	if (num_trans_thinking == 1 && num_trans_backlog == 0) {
		# a thinking user has made a successful transmission
		num_suc_trans <- num_suc_trans + 1
	} else if (num_trans_thinking == 0 && num_trans_backlog == 1) {
		# a backlogged user has made a successful transmission
		num_thinking <- num_thinking + 1
		num_backlog <- num_backlog - 1
		num_suc_trans <- num_suc_trans + 1
	} else { #if (num_trans_thinking >= 0 && num_trans_backlog >= 0) {
		# 1 or more thinking user collided with 1 or more backlogged users
		num_thinking <- num_thinking - num_trans_thinking
		num_backlog <- num_backlog + num_trans_thinking
	}
	
	##for (j in (1:n)) {
	#	if (clients[j] == THINKING) {
	#		transmissions[j] <- rbinom(1, 1, prob=p)	
	#	} else { # clients[j] == BACKLOGGED
	#		transmissions[j] <- rbinom(1, 1, prob=sigma)
	#	}
	#}
	#
	#num_trans <- sum(transmissions == 1)
#
	#if (num_trans == 1) { # No collisions
	#	channel_use <- channel_use + 1
	#	j <- match(1, transmissions)
	#	client[j] <- THINKING
	#} else if (num_trans == 0) { # no transmission
#
	#} else { #  transmission collided
	#	channel_use <- channel_use + 1
	#	for (j in (1:n)) {
	#		if (transmissions[j] == 1) {
	#			clients[j] <- BACKLOGGED
	#		}
	#	}
	#}
}

throughput <- num_suc_trans / total_slots
print(paste("Simulated Throughput =", throughput))
