# Libraries install.package("lib-name")
library(ggplot2)
library(pracma)

# constants
THINKING <- 0
BACKLOGGED <- 1
s_size <- 1000 

#sigma <- 0.1
sigmas <- logseq(1, 1.3, s_size) - 1
p <- 0.4
n <- 10

calc_theo_throughput <- function(n, sigma, p) {
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
	theo_throughput
}

cacl_sim_throughput <- function(n, simga, p) {
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
	}

	throughput <- num_suc_trans / total_slots
	#print(paste("Simulated Throughput =", throughput))
	throughput
}

make_ThX <- function(n, sigmas, p) {
	ThX <- c()

	for (sigma in sigmas) {
		ThX <- c(ThX, cacl_sim_throughput(n, sigma, p))
	}
	ThX
}

Th03 <- make_ThX(10, sigmas, 0.3)
Th04 <- make_ThX(10, sigmas, 0.4)
Th05 <- make_ThX(10, sigmas, 0.5)
Th06 <- make_ThX(10, sigmas, 0.6)

df <- data.frame(x=sigmas, y=c(Th03, Th04, Th05, Th06), p=c(rep("0.3", s_size), rep("0.4", s_size), rep("0.5", s_size), rep("0.6", s_size)))
g <- ggplot(df, aes(x=x, y=y, color=p)) + geom_line()
g1 <- g + scale_x_log10() + xlab("sigma") + ylab("Throughput")
print(g1)

calc_theo_throughput(n, sigma, p)
cacl_sim_throughput(n , sigma, p)