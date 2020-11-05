# my_data <- read.table("z:\\Documents\\MEIC\\SEMESTRE3\\DDRS\\labs\\lab3\\data\\2dtmcdata.txt", sep="\t")
my_data <- read.table("/Users/enrico/Desktop/2dtmcdata.txt", sep="\t")

est_trans_prob <- function(my_data) {
	trans_count <- matrix(0, nrow=2, ncol=2)
	
	for (i in (2:nrow(my_data))) {
		prev_state <- my_data[i - 1,]
		curr_state <- my_data[i,]
		trans_count[prev_state + 1, curr_state + 1] = trans_count[prev_state + 1, curr_state + 1] + 1
	}
	
	rel_freq <- matrix(0, nrow=2, ncol=2)

	for (i in (1:2)) {
		for (j in (1:2)) {
			rel_freq[i,j] <- trans_count[i,j] / sum(trans_count[,i])
		}
	}
    
	alpha <- rel_freq[1,1]
	beta <- rel_freq[2,1]
	c(alpha, beta)
}

x <- est_trans_prob(my_data)
print(paste("alpha =", x[1]))
print(paste("beta =", x[2]))