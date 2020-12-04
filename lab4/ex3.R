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

# Given a link, the function returns the sum of all arrival rates of all flows
# that traverse that given link
sum_all_flow_rates_in_link <- function(link) {
	lambda <- 0
	for (flow in Flows) {
		if (is.element(link, flow[["route"]])) {
			lambda <- lambda + flow[["rate"]]
		}
	}
	lambda
}

# Given a link, this function returns the average packet size in that given link
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

# Given a flow (identified by a flow_id, an integer from 1 to the number of flows)
# this function returns the average packet delay in that flow
calc_avg_pkt_delay_in_flow <- function(flow_id) {
	sum <- 0
	for (link in Flows[[flow_id]][["route"]]) {
		lambda <- sum_all_flow_rates_in_link(link)
		sum <- sum + (1 / ((LinkCapacities[link] / Flows[[flow_id]][["packetsize"]]) - lambda))
	}
	sum
}

# This fucntion returns the average number of packet in the system
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

# Given an average number of packets in the system (L), this function uses 
# Little's Law and returns the average packet delay in the system
calc_avg_pkt_delay_in_sys <- function(L) {
	lambda <- 0

	for (flow in Flows) {
		lambda <- lambda + flow[["rate"]]
	}
	L / lambda
}


paramters()

for (i in 1:length(Flows)) {
	print(paste("Average packet delay in flow", i, " = ", calc_avg_pkt_delay_in_flow(i), "sec."))
}

avg_pkts_in_sys <- calc_avg_pkts_in_sys()
avg_pkt_delay_in_sys <- calc_avg_pkt_delay_in_sys(avg_pkts_in_sys)
print(paste("Average number of packets in the system =", avg_pkts_in_sys, "packets"))
print(paste("Average packet delay in the system =", avg_pkt_delay_in_sys, "sec."))
 