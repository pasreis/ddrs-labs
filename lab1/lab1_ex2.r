generate_random_numbers <- function(n, seed) {
	set.seed(seed);
	runif(n);
}

x <- generate_random_numbers(5, 20);
y <- generate_random_numbers(5, 20);

result <- all(x == y);

print(result);