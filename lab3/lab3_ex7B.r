# Constants
IDLE <- 0
BUSY <- 1

arrival_rate <- c(1, 2)
service_rate <- 4

n <- 1000
time <- 0
num_queue_completed <- 0
num_system_completed <- 0
server_status <- IDLE
num_in_queue_1 <- 0
num_in_queue_2 <- 0
area_server_status <- 0
time_previous_event <- 0
acum_delay_queue <- 0
acum_delay_system <- 0
queue_1_arrival_time <- c()
queue_2_arrival_time <- c()
system_arrival_time_1 <- c()
system_arrival_time_2 <- c()
event_list <- c(rexp(1, arrival_rate[1]), rexp(1, arrival_rate[2]), Inf)

while (num_queue_completed < n) {
	# Extract next event
	next_event_type <- which.min(event_list) 
	time <- event_list[next_event_type]

	# Calculate area server status
	area_server_status <- area_server_status + server_status * (time - time_previous_event)

	if (next_event_type == 3) { # departure
		if (num_in_queue_1 == 0) { # No clients in queue 1
			# Extract from queue 2
			if (num_in_queue_2 == 0) { # No clients in queue 2
				# Set server to idle
				server_status <- IDLE
				event_list[3] <- Inf
			} else { # There are clients in queue 2
				# Calculate accumulated delay
				acum_delay_queue <- acum_delay_queue + (time - queue_2_arrival_time[1])

				# Extract from queue 2
				queue_2_arrival_time <- queue_2_arrival_time[-1]
				num_in_queue_2 <- num_in_queue_2 - 1
				num_queue_completed <- num_queue_completed + 1
				acum_delay_system <- acum_delay_system + (time - system_arrival_time_2[1])
				system_arrival_time_1 <- system_arrival_time_2[-1]

				# Schedule departure
				event_list[3] <- time + rexp(1, service_rate)
			}
		} else { # There are clients in queue 1
			# Calculate accumulated delay
			acum_delay_queue <- acum_delay_queue + (time - queue_1_arrival_time[1])
			
			# Extract from queue 1
			queue_1_arrival_time <- queue_1_arrival_time[-1]
			num_in_queue_1 <- num_in_queue_1 - 1
			num_queue_completed <- num_queue_completed + 1
			acum_delay_system <- acum_delay_system + (time - system_arrival_time_1[1])
			system_arrival_time_1 <- system_arrival_time_1[-1]

			# Schedule departure
			event_list[3] <- time + rexp(1, service_rate)
		}
		num_system_completed <- num_system_completed + 1
	} else { # Arrival
		if (next_event_type == 1) { # Client arrived to queue 1
			# Schedule next arrival
			event_list[1] <- time + rexp(1, arrival_rate[1])

			if (server_status == BUSY) {
				# Put client in queue 1
				queue_1_arrival_time <- c(queue_1_arrival_time, time)
				num_in_queue_1 <- num_in_queue_1 + 1
				system_arrival_time_1 <- c(system_arrival_time_1, time)
			} else { # Server is idle
				# Register arrival
				system_arrival_time_1 <- c(system_arrival_time_1, time)

				# Put client in server
				num_queue_completed <- num_queue_completed + 1
				server_status <- BUSY

				# Schedule departure
				event_list[3] <- time + rexp(1, service_rate)
			}
		} else { # Client arrived to queue 2
			# Schedule next arrival
			event_list[2] <- time + rexp(1, arrival_rate[2])

			if (server_status == BUSY) {
				# Put client in queue 2
				queue_2_arrival_time <- c(queue_2_arrival_time, time)
				num_in_queue_2 <- num_in_queue_2 + 1
				system_arrival_time_2 <- c(system_arrival_time_2, time)
			} else { # Server is idle
				# Register arrival
				system_arrival_time_2 <- c(system_arrival_time_2, time)
				
				# Put client in server
				server_status <- BUSY

				# Schedule departure
				event_list[3] <- time + rexp(1, service_rate)
			}
		}
	}
	time_previous_event <- time
}

avg_delay_queue <- acum_delay_queue / num_queue_completed
avg_delay_system <- acum_delay_system / time
server_utilization <- area_server_status / num_system_completed

print(paste("Average delay in queue", avg_delay_queue, "units of time"))
print(paste("Average delay in system", avg_delay_system, "units of time"))
print(paste("Server utilization", server_utilization * 100, "%"))