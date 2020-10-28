HEAD <- 1;
TAIL <- 0;

# This function will toss n coins with p probability of getting an head
toss_n_coins <- function(n, p) {
	x <- runif(5);
	res <- ifelse(x<0.7, HEAD, TAIL);
	res;
}

# Given a vector x with the results of one experiment, this function counts the number of heads
count_heads <- function(x) {
	n <- 0;
	for (value in x) {
		if (value == HEAD) {
			n <- n + 1;
		}
	}
	n;
}

# This function tosses n_coins coins n_times times with p probabilbity of getting an head
experiment_n_times <- function(n_times, n_coins, p) {
	n_success <- 0;
	i <- 0;
	while (i < n_times) {
		toss_result <- toss_n_coins(n_coins, p);
		n_heads <- count_heads(toss_result);
		if (n_heads == 2) {
			n_success <- n_success + 1;
		}
		i <- i + 1;
	}

	probability <- n_success / n_times;
	print(paste("Estimated Probability ", probability));
}

experiment_n_times(1000, 5, 0.7)
