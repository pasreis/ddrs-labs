# Libraries install.package("lib-name")
library(ggplot2)
library(gridExtra)

# Constants
SUCCESS <- 1
FAILURE <- 0

# Parameters
alpha <- 0.1
beta <- 0.6
p <- (1 - alpha) / (1 - alpha + beta)

gen_bernoulli <- function(n) {
	x <- runif(n)
	y <- ifelse(x < p, SUCCESS, FAILURE)
}

gen_2dmtc <- function(n) {
	curr_state <- ifelse(runif(1) < 0.5, SUCCESS, FAILURE)
	state_hist <- c(curr_state)

	for (i in (0:n)) {
		x <- runif(1)

		if (curr_state == SUCCESS) {
			curr_state <- ifelse(x < beta, FAILURE, SUCCESS)
		} else { # curr_state == FAILURE
			curr_state <- ifelse(x < alpha, SUCCESS, FAILURE)
		}

		state_hist <- c(state_hist, curr_state)
	}
	state_hist
}

sample_bernoulli <- gen_bernoulli(100)
sample_2dmtc <- gen_2dmtc(100)

p1 <- qplot(y=sample_2dmtc, ylab="2-DMTC")
p2 <- qplot(y=sample_bernoulli, ylab="Bernoulli")
p3 <- grid.arrange(p1, p2, ncol=1)
p3