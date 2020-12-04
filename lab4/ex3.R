paramters <- function() {  
	#Define here the capacity of each link
	LinkCapacities<<-c(100e6,100e6,200e6) #In bits/sec

	#Define here the flows. Flows is a list of lists that stores in each list the
	#arrival rate (in packets/second), the mean packet length (in bits) and the
	#route of each flow; the routes must be defined using the link identifiers
	#(and not the node identifiers)
	Flows<<-list(list(rate=7.5e3,packetsize=8000,route=c(1,3)),
				 list(rate=10e3,packetsize=8000,route=c(2,3)),
				 list(rate=5e3,packetsize=8000,route=c(3)))
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
		if (is.element(link, flow[["route"]])) {
			res <- c(res, flow)
		}
	}
	res
}

sum_all_flow_rates_in_link <- function(link) {
	lambda <- 0
	for (flow in Flows) {
		if (is.element(link, flow[["route"]])) {
			lambda <- lambda + flow[["rate"]]
		}
	}
	lambda
}

avg_pkt_size_in_link <- function(link) {
	pkt_size <- 0
	sum <- 0
	for (flow in Flows) {
		if (is.element(link, flow[["route"]])) {
			pkt_size <- pkt_size + flow[["packetsize"]]
			sum <- sum + 1
		}
	}
	pkt_size / sum
}

calc_avg_pkt_delay_in_flow <- function(flow_id) {
	sum <- 0
	for (link in Flows[[flow_id]][["route"]]) {
		flows_in_link <- get_all_flows_in_link(link)
		lambda <- sum_all_flow_rates_in_link(link)
		sum <- sum + (1 / ((LinkCapacities[link] / Flows[[flow_id]][["packetsize"]]) - lambda))
	}
	sum
}

calc_avg_pkts_in_sys <- function() {
	res <- 0
	num_links <- length(LinkCapacities)

	for (link in 1:num_links) {
		lambda <- sum_all_flow_rates_in_link(link)
		mu <- LinkCapacities[link] / avg_pkt_size_in_link(link)
		res <- res + (lambda / (mu - lambda))
	}
	res
}

calc_avg_pkt_delay_in_sys <- function(L) {
	lambda <- 0

	for (flow in Flows) {
		lambda <- lambda + flow[["rate"]]
	}
	L / lambda
}

#collect_input()
paramters()

for (i in 1:length(Flows)) {
	print(paste("Average packet delay in flow", i, " = ", calc_avg_pkt_delay_in_flow(i), "sec."))
}

avg_pkts_in_sys <- calc_avg_pkts_in_sys()
avg_pkt_delay_in_sys <- calc_avg_pkt_delay_in_sys(avg_pkts_in_sys)
print(paste("Average number of packets in the system =", avg_pkts_in_sys, "packets"))
print(paste("Average packet delay in the system =", avg_pkt_delay_in_sys, "sec."))
 