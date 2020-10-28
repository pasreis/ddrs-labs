# Constants
ARRIVAL = 1
DEPARTURE = 2
BUSY = 1
IDLE = 0

run = function(lambda, mu, n) {
	# Setup
	time = 0
	num_queue_completed = 0
	server_status = IDLE
	num_in_queue = 0
	acum_delay = 0
	queue_arrival_time = c()
	event_list = c(rexp(1, mu), Inf)
	
	# Simulation Cycle
	while (num_queue_completed < n) {
		next_event_type = which.min(event_list)
		time = event_list[next_event_type]
		
		if (next_event_type == ARRIVAL) {
			# Schedule next arrival
			event_list[1] = time + rexp(1, mu)

			if (server_status == BUSY) {
				queue_arrival_time = c(queue_arrival_time, time)
				num_in_queue = num_in_queue + 1
			} else { # Server is idle
				num_queue_completed = num_queue_completed + 1
				server_status = BUSY

				# Schedule next departure
				event_list[2] = time + rexp(1, lambda)
			}
		} else { # Departure
			if (num_in_queue == 0) {
				server_status = IDLE
				event_list[2] = Inf
			} else { # There are clients in the queue
				# Calculte accumulated delay in queue
				acum_delay = acum_delay + (time - queue_arrival_time[1])

				# Remove client from queue
				queue_arrival_time = queue_arrival_time[-1]
				num_in_queue = num_in_queue - 1
				num_queue_completed = num_queue_completed + 1
				
				# Schedule next departure
				event_list[2] = time + rexp(1, lambda)
			}
		}
	}
	print(paste("num_queue_completed =", num_queue_completed))
	avg_delay = acum_delay / num_queue_completed
}

print_stats = function(n, vector, conf_int) {
	print(vector)
	pop_mean = mean(vector)
	pop_var = var(vector)
	alpha_2 = (1 - conf_int) / 2
	t = qt(1 - alpha_2, df=(n - 1))
	l_12 = t * sqrt(pop_var / n)
	low = pop_mean - l_12
	high = pop_mean + l_12

	print(paste("Average delay in queue is ", pop_mean))
	print(paste(conf_int * 100, "% confidence interval is [", low, ",", high,"]"))
}

simulate = function(lambda, mu, n, n_runs, conf_int) {
	print(paste("Running simulation with lambda =", lambda, " mu =", mu, " simulation length =", n))
	avg_delays = c()
	theo_avg_delay = lambda / (mu * (mu - lambda))

	for (i in c(1:n_runs)) {
		avg_delays = c(avg_delays, run(lambda, mu, n))
	}

	print(paste("Theoretical average delay in queue is ", theo_avg_delay))
	print_stats(n, avg_delays, conf_int)
}

simulate(3, 4, 20, 25, 0.95)
print("")
simulate(3, 4, 2000, 25, 0.95)