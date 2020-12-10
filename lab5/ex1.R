# Libraries install.packages("lib-name")
require(lpSolve)

#      x_124  x_1234  x_134  x_24  x_234  x_123  x_13  r
C <- c(    0,      0,     0,    0,     0,     0,    0, 1)
#             x_124  x_1234  x_134   x_24  x_234  x_123    x_13    r
A <- matrix(c(    1,      1,     1,     0,     0,     0,     0,    0,  # Flow 1
                  0,      0,     0,     1,     1,     0,     0,    0,  # Flow 2
                  0,      0,     0,     0,     0,     1,     1,    0,  # Flow 4
			  4.5e6,  4.5e6,     0,     0,     0, 1.5e6,     0, -1e7,  # Link 1-2
				  0,      0, 4.5e6,     0,     0,     0, 1.5e6, -1e7,  # Link 1-3
				  0,  4.5e6,     0,     0, 2.5e6, 1.5e6,     0, -1e7,  # Link 2-3
		      4.5e6,      0,     0, 2.5e6,     0,     0,     0, -1e7,  # Link 2-4
				  0,  4.5e6, 4.5e6,     0, 2.5e6,     0,     0, -1e7), # Link 3-4
				  nrow=8, byrow=TRUE)

B <- c(1, 1, 1, 0, 0, -4.5e6, 0, 0)
cd <- c("=", "=", "=", "<=", "<=", "<=", "<=", "<=")

print("Solution with biforcated routing")
br <- lp(direction="min", objective.in=C, const.mat=A, const.dir=cd, const.rhs=B)
print(br$solution)
print(br$objval)

print("Solution with non-biforcated routing")
bin <- c(1, 2, 3, 4, 5, 6, 7)
nbr <- lp(direction="min", objective.in=C, const.mat=A, const.dir=cd, const.rhs=B, binary.vec=bin)
print(nbr$solution)
print(nbr$objval)