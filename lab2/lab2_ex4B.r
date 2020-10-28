# Constants
NORM = 0;
T = 1;

values = c(5, 10, 20, 50);
dist = c("Exponential", "Lognormal (0,1)", "Lognormal (0,2)", "Standard Normal");

calc_norm = function(i, dist) {
	critical = 0;
	
	if (dist == NORM) {
		critical = qnorm(0.975);
	} else {
		d_freedom = values[i] - 1;
		critical = qt(0.975, df=d_freedom);
	}

	mu = 0;
	count = 0;

	for (j in c(1:1000)) {
		x = rnorm(values[i], mean=0, sd=1);
		est_mean = mean(x);
		est_var = var(x);

		low = est_mean - critical * sqrt(est_var / values[i]);
		high = est_mean + critical * sqrt(est_var / values[i]);
		
		if (mu >= low && mu <= high) {
			count = count + 1;
		}
	}

	count / 1000;
}

calc_exp = function(i, dist) {
	critical = 0;

	if (dist == NORM) {
		critical = qnorm(0.975);
	} else {
		d_freedom = values[i] - 1;
		critical = qt(0.975, df = d_freedom);
	}

	lamda = 1;
	count = 0;

	for (j in c(1:1000)) {
		x = rexp(values[i]);
		est_mean = mean(x);
		est_var = var(x);

		low = est_mean - critical * sqrt(est_var / values[i]);
		high = est_mean + critical * sqrt(est_var / values[i]);

		if (lamda >= low && lamda <= high) {
			count = count + 1;
		}
	}
	count / 1000;
}

calc_lnorm = function(i, m, sdev, dist) {
	critical = 0;

	if (dist == NORM) {
		critical = qnorm(0.975);
	} else {
		d_freedom = values[i] - 1;
		critical = qt(0.975, df=d_freedom);
	}

	count = 0;

	for (j in c(1:1000)) {
		x = rlnorm(values[i], meanlog=m, sdlog=sdev);
		est_mean = mean(x);
		est_var = var(x);

		low = est_mean - critical * sqrt(est_var / values[i]);
		high = est_mean + critical * sqrt(est_var / values[i]);

		if (m >= low && m <= high) {
			count = count + 1;
		}
	}
	count / 1000;
}

# Make a table for output using Normal Distribution
T_norm = table(Distribution=dist, N=values);

# Make a table for output using Student's t Distribution
T_t = table(Distribution=dist, N=values);

# Fill the table
for (i in c(1:4)) {
	T_norm[1,i] = calc_exp(i, NORM);
	T_t[1,i] = calc_exp(i, T);

	T_norm[2,i] = calc_lnorm(i, 0, 1, NORM);
	T_t[2,i] = calc_lnorm(i, 0, 1, T);

	T_norm[3,i] = calc_lnorm(i, 0, 2, NORM);
	T_t[3,i] = calc_lnorm(i, 0, 2, T);

	T_norm[4,i] = calc_norm(i, NORM);
	T_t[4,i] = calc_norm(i, T);
}

print("Estimations using the Normal Distribution");
print(T_norm);
print("");
print("Estimations using the Student's t Distribution");
print(T_t);

