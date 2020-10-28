p <- 0.2;
success <- 1;
fail <- 0;
x <- runif(1000);
y <- ifelse(x > p, success, fail);
hist(y);
