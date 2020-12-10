# Libraries install.packages("lib-name")
require(lpSolve)

#      x_12  x_13  x_23  x_24  x_34
C <- c(   1,    5,    1,    5,    1)
#             x_12  x_13  x_23  x_24  x_34
A <- matrix(c(   1,    1,    0,    0,    0,
                -1,    0,    1,    1,    0,
				 0,   -1,   -1,    0,    1,
				 0,   -1,    0,    0,   -1),
			nrow=4, byrow=TRUE)

B <- c(1, 0, 0, -1)
cd <- c("=", "=", "=", "=")
bin <- c(1:5)
sp <- lp(direction="min", objective.in=C, const.mat=A, const.dir=cd, const.rhs=B, binary.vec=bin)
print(sp$solution)
print(sp$objval)