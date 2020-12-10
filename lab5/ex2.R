# Libraries install.packages("lib-name")
require(lpSolve)

#      x_124  x_1234  x_134  x_24  x_234  x_123  x_13  y_12  y_13  y_23  y_34  y_24
C <- c(    0,      0,     0,    0,     0,     0,    0, 1000, 1000, 1000, 1000, 1000)

#             x_124  x_1234  x_134   x_24  x_234  x_123   x_13  y_12  y_13  y_23  y_34  y_24
A <- matrix(c(    1,      1,     1,     0,     0,     0,     0,    0,    0,    0,    0,    0,  # Flow 1
                  0,      0,     0,     1,     1,     0,     0,    0,    0,    0,    0,    0,  # Flow 2
				  0,      0,     0,     0,     0,     1,     1,    0,    0,    0,    0,    0,  # Flow 3
			  4.5e6,  4.5e6,     0,     0,     0, 1.5e6, 1.5e6, -2e6,    0,    0,    0,    0,  # Link 1-2
			      0,      0, 4.5e6,     0,     0,     0, 1.5e6,    0, -2e6,    0,    0,    0,  # Link 1-3
				  0,  4.5e6,     0,     0, 2.5e6, 1.5e6,     0,    0,    0, -2e6,    0,    0,  # Link 2-3
			  4.5e6,      0,     0, 2.5e6,     0,     0,     0,    0,    0,    0,    0, -2e6,  # Link 2-4
			      0,  4.5e6, 4.5e6,     0, 2.5e6,     0,     0,    0,    0,    0, -2e6,    0), # Link 3-4
				  nrow=8, byrow=TRUE)

B <- c(1, 1, 1, 0, -1.5e6, -4.5e6, 0, 0)
cd <- c("=", "=", "=", "<=", "<=", "<=", "<=", "<=")
int <- c(8, 9, 10, 11, 12)

print("Solution with biforcated routing")
br <- lp(direction="min", objective.in=C, const.mat=A, const.dir=cd, const.rhs=B, int.vec=int)
print(br$solution)
print(br$objval)

print("Solution with non-biforcated routing")
bin <- c(1, 2, 3, 4, 5, 6, 7)
br <- lp(direction="min", objective.in=C, const.mat=A, const.dir=cd, const.rhs=B, int.vec=int, binary.vec=bin)
print(br$solution)
print(br$objval)