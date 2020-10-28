x <- rexp(1000, rate=0.5);
hist(x);
t_0 <- 0;

#count_arrivals <- function(t) {
#  count <- 0;
#  for(i in 2:1000){
#	if(t[i]!=t[i-1]) {
#		count <- count + 1; 
#	}
#  }
#  count;
#}

calc_intervals <- function() {
	t <- cumsum(x);
}

t <- calc_intervals();
#arr <- count_arrivals(t);
est <- 1000 / t[1000];
print(paste("Estimated lambda = ", est));