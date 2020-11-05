# Libraries install.package("lib-name")
library(ggplot2)
library(gridExtra)

# Constants
SUCCESS <- 1
FAILURE <- 0

# Parameters
alpha <- 0.9
beta <- 0.1
p <- (1 - alpha) / (1 - alpha + beta)

gen_bernoulli <- function(n) {
	rbinom(n, 1, prob=p)
}

gen_2dtmc <- function(n) {
	curr_state <- ifelse(runif(1) < 0.5, SUCCESS, FAILURE)
	state_hist <- c(curr_state)

	for (i in (0:n)) {
		x <- runif(1)

		if (curr_state == 1) {
			curr_state <- ifelse(x < beta, 0, 1) 
		} else { # curr_state == 0
			curr_state <- ifelse(x < alpha, 0, 1)
		}

		state_hist <- c(state_hist, curr_state)
	}
	state_hist
}

sample_bernoulli <- gen_bernoulli(200)
sample_2dtmc <- gen_2dtmc(200)

p1 <- qplot(y=sample_2dtmc, ylab="2-DTMC")
p2 <- qplot(y=sample_bernoulli, ylab="Bernoulli")
p3 <- grid.arrange(p1, p2, ncol=1)
p3