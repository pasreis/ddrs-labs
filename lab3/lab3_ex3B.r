# constants
THINKING <- 0
BACKLOGGED <- 1

calc_throughput = function(n,sigma,p){

    states <- c(0:n)

    P <- matrix(0, nrow=n+1, ncol=n+1)

    for (i in (0:n)) {
        for (j in (0:n)) {
            if (j < i - 1) {
                P[i+1, j+1] <- 0
            } else if (j == i - 1) {
                P[i+1, j+1] <- i * p * (1 - p) ^ (i - 1) * (1 - sigma)^(n - i)
            } else if (j == i) {
                P[i+1, j+1] <- (1 - sigma) ^ (n - i) * (1 - i * p * (1 - p) ^ (i - 1)) + (n - i) * sigma * (1 - sigma) ^ (n - i - 1) * (1 - p) ^ i
            } else if (j == i + 1) {
                P[i+1, j+1] <- (n - i) * sigma * (1 - sigma) ^ (n - i - 1) * (1 - (1 - p) ^ i)
            } else if (j > i + 1) {
                P[i+1, j+1] <- dim(combn(n - i, j - i))[2] * sigma ^ (j - i) * (1 - sigma) ^ (n - j)
            }
        }
    }

    state <- 0
    num_suc_trans <- 0

    E <- matrix(1, nrow=n+1, ncol=n+1)
    I <- diag(n + 1)
    vec <- rep(1, n + 1)
    inverse <- solve(P + E - I)
    Pi <- c()
    for (i in (0:n+1)) { # even starting from 0 index start from 1
        Pi <- c(Pi, sum(inverse[,i]))
    }

    theo_throughput <- 0
    for (i in (0:n)) {
        p_suc <- (n - i) * sigma * (1 - sigma) ^ (n - i - 1) * (1 - p) ^ (i) + i * p * (1 - p) ^ (i - 1) * (1 - sigma) ^ (n - i)
        theo_throughput <- theo_throughput + (p_suc * Pi[i + 1])
    }

    print(paste("Theoretical throughput", theo_throughput))
    theo_throughput
}

#B) Plot the theoretical throughput as a function of sigma
# 𝑁 = 10 and 𝑝 = 0.3, 𝑝 = 0.4, 𝑝 = 0.5, and 𝑝 = 0.6. 
# How do you explain the evolution of the throughput with these two parameters?

sigmas=logseq(1,1.3,1000)-1
pvec = c(0.3,0.4,0.5,0.6)

Th03 = c()
Th03 = c(Th03,calc_throughput(10,0.1,0.3))
Th03 = c(Th03,calc_throughput(10,0.2,0.3))
Th03 = c(Th03,calc_throughput(10,0.3,0.3))
Th03 = c(Th03,calc_throughput(10,0.4,0.3))

Th04 = c()
Th04 = c(Th04,calc_throughput(10,0.1,0.4))
Th04 = c(Th04,calc_throughput(10,0.2,0.4))
Th04 = c(Th04,calc_throughput(10,0.3,0.4))
Th04 = c(Th04,calc_throughput(10,0.4,0.4))

Th05 = c()
Th05 = c(Th05,calc_throughput(10,0.1,0.5))
Th05 = c(Th05,calc_throughput(10,0.2,0.5))
Th05 = c(Th05,calc_throughput(10,0.3,0.5))
Th05 = c(Th05,calc_throughput(10,0.4,0.5))

Th06 = c()
Th06 = c(Th06,calc_throughput(10,0.1,0.6))
Th06 = c(Th06,calc_throughput(10,0.2,0.6))
Th06 = c(Th06,calc_throughput(10,0.3,0.6))
Th06 = c(Th06,calc_throughput(10,0.4,0.6))


df=data.frame(x=sigmas,y=c(Th03,Th04,Th05,Th06),p=c(rep("0.3" ,s_size),rep("0.4",s_size),rep("0.5",s_size),rep("0.6",s_size)))
g=ggplot(df,aes(x=x,y=y,color=p)) + geom_line()
g1=g + scale_x_log10() + xlab("sigma") + ylab("Throughput")

plot(g1)

#plot(pvec, throughputs, main="Theoretical Throughput as a Function of p:\n
#        N=10, sigma=0.1", xlab="p", ylab="throughput", type="o")

# throughput should decrease because backlogged users will be more likely to transmit
# and therefore to collide
