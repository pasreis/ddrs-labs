BLUE <- 0;
YELLOW <- 1;

blue_balls_urn_1 <- 4;
yellow_balls_urn_1 <- 2;
blue_balls_urn_2 <- 3;
yellow_balls_urn_2 <- 5;

draw_urn_1 <- function() {
	p <- blue_balls_urn_1 / (blue_balls_urn_1 + yellow_balls_urn_1);
	res <- ifelse(runif(1) > p, YELLOW, BLUE);
	if (res == BLUE) {
		blue_balls_urn_2 <- blue_balls_urn_2 + 1;
		blue_balls_urn_1 <- blue_balls_urn_1 - 1;
	} else {
		yellow_balls_urn_2 <- yellow_balls_urn_2 + 1;
		yellow_balls_urn_1 <- yellow_balls_urn_1 - 1;
	}
	res;
}

draw_urn_2 <- function() {
	p <- blue_balls_urn_2 / (blue_balls_urn_2 + yellow_balls_urn_2);
	ifelse(runif(1) > p, YELLOW, BLUE);
}

experiment_n_times <- function(n) { 
	i <- 0;
	n_blue <- 0;
	while (i < n) {
		draw_urn_1();
		ball <- draw_urn_2();
		if (ball == BLUE) {
			n_blue <- n_blue + 1;
		}
		i <- i + 1;
	}
	p <- n_blue / n;
	print(paste("The Estimated Probability is ", p));
}

experiment_n_times(1000);
