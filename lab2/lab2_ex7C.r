# Constants
ARRIVAL = 1
DEPARTURE = 2
BUSY = 1
IDLE = 0

# Variables

set_up = function(mu) {
	time <<- 0
	num_queue_completed <<- 0
	server_status <<- IDLE
	num_in_queue <<- 0
	acum_delay <<- 0
	queue_arrival_time <<- c()
	event_list <<- c(rexp(1, mu), Inf)
	warm_up_count <<- 0
	delays_in_queue <<- c()
	n <<- 0
	num_replicas <<- 0
}

timing = function() {
	next_event_type <<- which.min(event_list)
	time <<- event_list[next_event_type]
}

push_queue = function() {
	queue_arrival_time <<- c(queue_arrival_time, time)
	num_in_queue <<- num_in_queue + 1
}

pop_queue = function() {
	queue_arrival_time <<- queue_arrival_time[-1]
	num_in_queue <<- num_in_queue - 1
	num_queue_completed <<- num_queue_completed + 1
}

skip_queue = function() {
	num_queue_completed <<- num_queue_completed + 1
	server_status <<- BUSY
}

count_delay = function() {
	delays_in_queue <<- c(delays_in_queue, (time - queue_arrival_time[1]))
	n <<- n + 1
}

reset_delays = function() {
	delays_in_queue <<- c()
	n <<- 0
}

schedule = function(type, param) {
	event_list[type] <<- time + rexp(1, param)
}

do_arrival = function(mu, lambda) {
	schedule(ARRIVAL, mu)

	if (server_status == BUSY) {
		push_queue()
	} else { # server is IDLE
		skip_queue()
		schedule(DEPARTURE, lambda)
	}
}

do_departure = function(mu, lambda) {
	if (num_in_queue == 0) {
		server_status <<- IDLE
		event_list[2] <<- Inf
	} else {
		count_delay()
		pop_queue()
		schedule(DEPARTURE, lambda)
	}
}

do_stats = function(lambda, mu) {
	theo_avg_delay <<- lambda / (mu * (mu - lambda))
	num_replicas <<- num_replicas + 1
	pop_mean <<- mean(delays_in_queue)
	pop_var <<- var(delays_in_queue)
	t <- qt(0.975, df=(num_queue_completed - 1))
	l_12 <<- t * sqrt(pop_var / num_queue_completed)
	gama <<- abs(pop_mean - theo_avg_delay) / abs(theo_avg_delay)
}

report_stats = function() {
	values <- c(theo_avg_delay, num_replicas, pop_mean, pop_var, l_12, gama, n_0, num_replicas, mu, lamda)
}

run = function(lambda, mu, n_0) {
	set_up(mu)
	
	while(n <= n_0) {
		timing()

		if (next_event_type == ARRIVAL) {
			do_arrival(lambda, mu)
		} else { # DEPARTURE
			do_departure(lambda, mu)
		}	
	}

	repeat {
		timing()

		if (next_event_type == ARRIVAL) {
			do_arrival(lambda, mu)
		} else { # DEPARTURE
			do_departure(lambda, mu)
		}

		do_stats(lambda, mu)		

		if ((l_12 / pop_mean) <= (gama / (1 + gama))) {
			break
		}
	}
	
	report_stats()
}

run(1,2,20)