PROB <- 0.8;
STEP <- 1;

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

calc_result <- function(n) {
  est <- vector(mode="double",0);
  z_e <- c(1:500);
  i <- 1;
  currentEstimation <- 0;
  while (i <= 500) {
    exp <- generate_N_bernoulli(n,PROB);
    z_e[i] <- as.numeric(count_successes(exp)/n);
    i <- i+1;
  }

  theo_var <- ((PROB * (1 - PROB)) / n);
  est_mean <- mean(z_e);
  est_var <- var(z_e);

  print(paste("The obtained results with ", n, "experiments are:"));
  print(paste("  Theoritical Mean = ", PROB));
  print(paste("  Theoritical Variance = ", theo_var));
  print(paste("  Estimated Mean = ", est_mean));
  print(paste("  Estimated Variance = ", est_var));
  
}

calc_result(100);
calc_result(500);
calc_result(1000);
calc_result(10000);