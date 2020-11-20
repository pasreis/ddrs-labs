# Constants
ARRIVAL <- 1
POISSON <- 2 
UNI     <- 3     

schedule_arr <- function(lambda, meth, time) {
	if (meth == POISSON) {
		time <- time + rexp(1, lambda)
	} else if (meth == UNI) {
		time <- time + runif(1, min=2, max=4)
	}
	time
}

simulate <- function(n, lambda, arr_method) {
	num_clients_in_sys <- 0
	event_list <- c(Inf, Inf)
	time <- 0
	time_prev_event <- 0
	num_sys_completed <- 0
	a <- rep(0, 3)
	p <- rep(0, 3)

	# Schedule next arrival
	event_list[1] <- schedule_arr(lambda, arr_method, time)

	while (num_sys_completed < n) {
		next_event_type <- which.min(event_list)
		time <- event_list[next_event_type]

		if (next_event_type == ARRIVAL) {
			# Schedule departure
			event_list <- c(event_list, time + 1)

			# Schedule next arrival
			event_list <- c(event_list, schedule_arr(lambda, arr_method, time))

			# Count clients in system
			if (num_clients_in_sys <= 3 && num_clients_in_sys > 0) {
				a[num_clients_in_sys] <- a[num_clients_in_sys] + 1
				p[num_clients_in_sys] <- p[num_clients_in_sys] + (time - time_prev_event)
			}

			# Increment clients in system
			num_clients_in_sys <- num_clients_in_sys + 1

		} else { # DEPARTURE
			# Remove departure from event list
			event_list <- event_list[-2]
			
			# Count clients in system
			if (num_clients_in_sys < 3 && num_clients_in_sys > 0) {
				p[num_clients_in_sys + 1] <- p[num_clients_in_sys + 1] + (time - time_prev_event)
			}

			# Decrement clients in system
			num_clients_in_sys <- num_clients_in_sys - 1
			
			# Count clients that traversed the system
			num_sys_completed <- num_sys_completed + 1
		}

		time_prev_event <- time
	}

	
	if (arr_method == POISSON) {
		print("Using method: Poisson")
	} else {
		print("Using method: Uniform")
	}

	print("Proportion of clients that upon arrival see:")
	for (i in (1:3)) {
		a[i] <- a[i] / (num_sys_completed + num_clients_in_sys)
		print(paste("    ", i - 1, "clients in the system:", a[i]))
	}
	
	print("Limiting state probabilitie of having:")	
	for (i in (1:3)) {
		p[i] <- p[i] / time
		print(paste("    ", i - 1, "clients in the system:", a[i]))
	}
}

simulate(1000, 1, POISSON)
simulate(1000, 1, UNI)
