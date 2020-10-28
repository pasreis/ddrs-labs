PROB <- 0.8;
STEP <- 10;

#sort of a B(n,p) but as a sequence of Bernoulli variables
generate_N_bernoulli <- function(n,p) {
  x <- runif(n);
  y <- ifelse(x<p,1,0);
  y;
}

#count how many successes have been generated in the binomial experiment
count_successes <- function(x) {
  count <- 0;
  for(value in x){
    if(value==1){ count <- count +1; }
  }
  count;
}

#increase N and print the estimated probability as a function of the number of the experiments
#it should tend to PROB, arbitrarilly set at the beginning of the script

#STEPS is used to speed up the process and could be changed,
# N goes from 1 experiment to 1000 experiments anyways
plot_result <- function() {
  est <- vector(mode="double",0);

  i <- 1;

  while(i<=1001) {
    expResults <- generate_N_bernoulli(i,PROB);
    currentEstimation <- count_successes(expResults)/i;
    print(paste(count_successes(expResults), " successi su ", i, " esperimenti\n"));
    print(paste("The estimated probability with ", i, " experiments is: ", currentEstimation));

    est <- append(est,currentEstimation);
    i <- i+STEP;
  }

  plot(est, type="l");
}

plot_result();
