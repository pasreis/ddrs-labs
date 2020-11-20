n <- 1500 # number of clients that traversed the queue

simulate <- function(n, arrival_rate, service_rate) {
	time <- 0
	num_queue_completed <- 0
	server_status <- 0
	num_in_queue <- 0
	acum_delay <- 0
	queue_arrival_time <- c()
	event_list <- c(rexp(1, arrival_rate), Inf)

	while (num_queue_completed < n) {
		next_event_type <- which.min(event_list)
		time <- event_list[next_event_type]

		if (next_event_type == 1) { # arrival
			event_list[1]= time + rexp(1, arrival_rate)
			
			if (server_status == 1) { # server is busy
				queue_arrival_time <- c(queue_arrival_time, time)
				num_in_queue <- num_in_queue + 1
			} else { # server is idle
				num_queue_completed <- num_queue_completed + 1
				server_status <- 1
				event_list[2] <- time + rexp(1, service_rate)
			}
		} else { # depature
			if (num_in_queue == 0) {
				server_status <- 0
				event_list[2] <- Inf
			} else {
				acum_delay <- acum_delay + (time - queue_arrival_time[1])
				queue_arrival_time <- queue_arrival_time[-1]
				num_in_queue <- num_in_queue - 1
				num_queue_completed <- num_queue_completed + 1
				event_list[2] <- time + rexp(1, service_rate)
			}
		}
	}
	avg_delay <- acum_delay / num_queue_completed
	theo_avg_delay <- arrival_rate / (service_rate * (service_rate - arrival_rate))

	print(paste("Theoretical Results with lambda =", arrival_rate, "and mu =", service_rate))
	print(paste("  Average Delay in Queue:", theo_avg_delay))

	print("")
	print(paste("Average Delay in Queue: ", avg_delay))
}

simulate(1000, 2, 2)
simulate(1000, 4, 2)