
collect_input <- function() {
	num_nodes <<- strtoi(readline(prompt="Number of nodes in the network: "))
	network <<- matrix(0, nrow=num_nodes, ncol=num_nodes)
	
	max_links <- num_nodes * num_nodes
	i <- 0

	while (TRUE) {
		print("Add new link to network (leave empty to exit):")
		src <- strtoi(readline(prompt="from: "))
		dst <- strtoi(readline(prompt="to: "))
		cap <- strtoi(readline(prompt="capacity (bits/s): "))

		if (src > 0 && src <= num_nodes && dst > 0 && dst <= num_nodes) {
			network[src, dst] <<- cap
			print(paste("Added a link from", src, "to", dst, "with capacity", cap, "bits/s"))
		} else {
			print("Invalid link, try again!")
		}

		exit <- readline(prompt="Do you want to add more links? (y/n) ")

		if (exit == "n") {
			break
		} else if (exit == "y") {
			next
		} else {
			print("Invalid option")
			quit(status=1)
		}
	}

	num_flows <<- strtoi(readline(prompt="Number of flows: "))
	flows <<- rep(list(1), num_flows)

	for (i in (1:num_flows)) {
		print(paste("Create a path for flow", i, "(leave empty to finish)"))
		path <- c()
		while (TRUE) {
			point <- strtoi(readline())

			if (is.na(point)) {
				break
			} else if (point > 0 && point <= num_nodes) {
				path <- c(path, point)
			} else {
				print("Invalid path, try again!")
			}
		}
		
		avg_pkg_len <- strtoi(readline(prompt=paste("Flow", i, "average packet length (bits): ")))
		arr_rate <- strtoi(readline(prompt=paste("Flow", i, "arrival rate (packets/s): ")))
		flows[[i]] <<- c(list(path), avg_pkg_len, arr_rate)
	}
}

get_path <- function(flow_id) {
	flows[[flow_id]][[1]]
}

get_avg_pkg_len <- function(flow_id) {
	flows[[flow_id]][[2]]
}

get_arr_rate <- function(flow_id) {
	flows[[flow_id]][[3]]
}

calc_theo_avg_pkg_delay_in_flow <- function(flow_id) {
	path <- get_path(flow_id)
	len_path <- length(path)
	i <- 1
	j <- i + 1
	sum <- 0

	while (j <= len_path) {
		sum <- sum + (1 / ((network[i,j] / get_avg_pkg_len(flow_id)) - get_arr_rate(flow_id)))
		i <- i + 1
		j <- i + 1
	}
	sum
}

get_all_flows_in_link <- function(i, j) {
	res <- c()
	for (k in 1:num_flows) {
		path <- get_path(k)
		path_len <- length(path)

		for (l in 1:(path_len - 1)) {
			if (path[l] == i && path[l + 1] == j) {
				res <- c(res, k)
			}
		}
	}
	res
}

calc_avg_pkgs_in_sys <- function() {
	res <- 0
	for (i in 1:num_nodes) {
		for (j in 1:num_nodes) {
			if (network[i,j] != 0) {
				flows_in_link <- get_all_flows_in_link(i, j)
				lambda <- 0
				
				for (flow_in_link in flows_in_link) {
					lambda <- lambda + get_arr_rate(flow_in_link)
				}

				avg_pkg_len_in_link <- 0

				for (flow_in_link in flows_in_link) {
					avg_pkg_len_in_link <- avg_pkg_len_in_link + get_avg_pkg_len(flow_in_link)
				}

				res <- res + (lambda / ((network[i,j] / avg_pkg_len_in_link) - lambda))
			}
		}
	}
	res
}

calc_theo_avg_pkg_delay_in_sys <- function(L) {
	lambda <- 0

	for (i in 1:num_flows) {
		lambda <- lambda + get_arr_rate(i)
	}

	L / lambda
}

collect_input()
avg_pkg_delay <- calc_theo_avg_pkg_delay_in_flow(1)
avg_pkg_in_sys <- calc_avg_pkgs_in_sys()
avg_delay_in_sys <- calc_theo_avg_pkg_delay_in_sys(avg_pkg_in_sys)

print("")
print("Network topology (matrix form):")
print(network)
print("")

for (i in 1:num_flows) {
	print(paste("Average packet delay in flow ", i , "=", avg_pkg_delay, "sec."))
}

print(paste("Average number of packets in the system =", avg_pkg_in_sys, "packets"))
print(paste("Average delay in system =", avg_delay_in_sys, "sec."))
 