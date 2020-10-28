dist <- runif(10000);
dist <- -log(dist, exp(1));
x <- seq(0, 12, 0.01);
y <- dexp(x, 1);

hist(dist, prob=TRUE);
lines(x,y);
