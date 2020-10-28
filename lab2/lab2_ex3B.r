pop = c(1.58, -1.04, 2.11, 0.41, 3.40, -0.34, -0.12, 3.14, 3.12, 1.54, 1.53, 2.76);

pop_df = length(pop) - 1;

critical_norm = qnorm(0.95);
critical_t = qt(0.95, df=pop_df);

print(paste("Critical value for normal distribution: ", critical_norm));
print(paste("Critical value for student's t distribution: ", critical_t));