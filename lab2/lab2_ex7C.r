source("lab2_ex7C_simulate.r")
#arrvial, server, num of deays, warm up, relative error = |X - mu| / |mu|
n_0 = 10
x = 0
avg_delays = c()
set.seed(81);

replication_deletion = function(n_0, k, ArrivalRate, ServiceRate, N, conf_int, gama) {
	theo_avg_delay = ArrivalRate / (ServiceRate * (ServiceRate - ArrivalRate))
	for (i in 1:(n_0)) {
		avg_delays = c(avg_delays, simulate(k, ArrivalRate, ServiceRate, N))
	}

	repeat {
		m = mean(avg_delays)
		v = var(avg_delays)
		t = qt(conf_int, df=n_0 + x - 1)
		l_12 = t * sqrt(v / (n_0 + x))
		low = m - l_12
		high = m + l_12

		print(paste("Theoretical Average Delay =", theo_avg_delay))
		print(paste("Average Delay =", m))
		print(paste("Confidence interval = [",low,",",high,"]"))
		print(paste("Error =", gama))
		if ((l_12 / m) <= (gama / (1 + gama))) {
			print(paste("Number of replications =", n_0 + x))
			break
		}
		avg_delays = c(avg_delays, simulate(k, ArrivalRate, ServiceRate, N))
		x = x + 1
	}
}
replication_deletion(10, 100, 1, 2, 2000, 0.95, 0.05)
#while()
# build confidence interval with warm up
# add replicas while stopping condition is not met