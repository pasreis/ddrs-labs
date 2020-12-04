paramters <- function() {  
	#Define here the capacity of each link
  LinkCapacities<<-c(64e3,64e3) #In bits/sec

	#Define here the flows. Flows is a list of lists that stores in each list the
	#arrival rate (in packets/second), the mean packet length (in bits) and the
	#route of each flow; the routes must be defined using the link identifiers
	#(and not the node identifiers)
	Flows<<-list(list(rate=32,packetsize=1000,route=c(1,2)))
}

collect_input <- function() {
	num_nodes <<- strtoi(readline(prompt="Number of nodes in the network: "))
	network <<- matrix(0, nrow=num_nodes, ncol=num_nodes)
	
	max_links <- num_nodes * num_nodes
	i <- 0

	while (TRUE) {
		print("Add new link to network (leave empty to exit):")
		src <- strtoi(readline(prompt="from: "))
		dst <- strtoi(readline(prompt="to: "))
		cap <- as.numeric(readline(prompt="capacity (bits/s): "))

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
		
		avg_pkt_len <- as.numeric(readline(prompt=paste("Flow", i, "average packet length (bits): ")))
		arr_rate <- as.numeric(readline(prompt=paste("Flow", i, "arrival rate (packets/s): ")))
		flows[[i]] <<- c(list(path), avg_pkt_len, arr_rate)
	}
}

get_path <- function(flow_id) {
	flows[[flow_id]][[1]]
}

get_avg_pkt_len <- function(flow_id) {
	flows[[flow_id]][[2]]
}

get_arr_rate <- function(flow_id) {
	flows[[flow_id]][[3]]
}

# Returns a vector with all flows that contain link in their routing
get_all_flows_in_link <- function(link) {
	res <- c()
	for (flow in Flows) {
		for (l in flow[["route"]]) {
			if (l == link) {
				res <- c(res, flow)
			}
		}
	}
	res
}

calc_theo_avg_pkt_delay_in_flow <- function(flow_id) {
	sum <- 0
	for (link in Flows[[flow_id]][["route"]]) {
		flows_in_link <- get_all_flows_in_link(link)
		lambda <- 0
		lambda <- sum(flows_in_link[["rate"]])
		sum <- sum + (1 / ((LinkCapacities[link] / Flows[[flow_id]][["packetsize"]]) - lambda))
	}
	sum
}

calc_avg_pkts_in_sys <- function() {
	res <- 0
	for (i in 1:num_nodes) {
		for (j in 1:num_nodes) {
			if (network[i,j] != 0) {
				flows_in_link <- get_all_flows_in_link(i, j)
				lambda <- 0
				
				for (flow_in_link in flows_in_link) {
					lambda <- lambda + get_arr_rate(flow_in_link)
				}

				avg_pkt_len_in_link <- 0

				for (flow_in_link in flows_in_link) {
					avg_pkt_len_in_link <- avg_pkt_len_in_link + get_avg_pkt_len(flow_in_link)
				}

				avg_pkt_len_in_link <- avg_pkt_len_in_link / length(flows_in_link)
				res <- res + (lambda / ((network[i,j] / avg_pkt_len_in_link) - lambda))
			}
		}
	}
	res
}

calc_theo_avg_pkt_delay_in_sys <- function(L) {
	lambda <- 0

	for (i in 1:num_flows) {
		lambda <- lambda + get_arr_rate(i)
	}

	L / lambda
}

#collect_input()
paramters()
avg_pkt_delay <- calc_theo_avg_pkt_delay_in_flow(1)
print(avg_pkt_delay)
#avg_pkt_in_sys <- calc_avg_pkts_in_sys()
#avg_delay_in_sys <- calc_theo_avg_pkt_delay_in_sys(avg_pkt_in_sys)

#print("")
#print("Network topology (matrix form):")
#print(network)
#print("")

#for (i in 1:num_flows) {
#	print(paste("Average packet delay in flow ", i , "=", calc_theo_avg_pkt_delay_in_flow(i), "sec."))
#}

#print(paste("Average number of packets in the system =", avg_pkt_in_sys, "packets"))
#print(paste("Average delay in system =", avg_delay_in_sys, "sec."))
 