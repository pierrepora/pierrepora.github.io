# Session 3 - Solutions (R script)
# September 2025
# Set seed for reproducibility where relevant
set.seed(12345)

### Exercise 1
# Create a vector of natural integers from 1 to 100 (ascending)
vec1 <- 1:100
vec1

### Exercise 2
# Vector with 100 repetitions of numbers 1 to 10, then 20 repetitions of 1
part1 <- rep(1:10, times = 100)   # length = 1000
part2 <- rep(1, times = 20)
vec2 <- c(part1, part2)
length(vec2)  # 1020
vec2
head(vec2); tail(vec2)

### Exercise 3
# Create vector 1..100 and compute sum, verify formula sum_{k=1}^n k = n(n+1)/2
n <- 100
v <- 1:n
sum_v <- sum(v)
formula <- n * (n + 1) / 2
sum_v; formula
all.equal(sum_v, formula)

### Exercise 4
# Vector 1..100 in descending order
vec_desc <- 100:1
vec_desc

### Exercise 5
# From ascending vector produce descending vector (two ways)
asc <- 1:100
desc1 <- rev(asc)
desc2 <- 100:1
all.equal(desc1, desc2)  # TRUE

### Exercise 6
# First 100 elements of u_{n+1} = 0.3 u_n + 5, with u1 = 3
u <- numeric(100)
u[1] <- 3
for (i in 2:100) {
  u[i] <- 0.3 * u[i-1] + 5
}
# Check all elements < 8
all_less_than_8 <- all(u < 8)
all_less_than_8
# Show first few
head(u, 10)

### Exercise 7
# Fibonacci sequence: u0 = 0, u1 = 1, u_{n+2} = u_n + u_{n+1}
# Construct vector with first 50 elements (we interpret "first 50 elements" as u0..u49)
fib <- numeric(50)
fib[1] <- 0  # u0
fib[2] <- 1  # u1
for (i in 3:50) {
  fib[i] <- fib[i-2] + fib[i-1]  # since R indexing: fib[3] = u2 = u0 + u1
}
fib

### Exercise 8
# Simulate 100 rolls of a fair six-sided die; compute empirical mean and sd
rolls <- sample(1:6, size = 100, replace = TRUE)
mean_rolls <- mean(rolls)
sd_rolls <- sd(rolls)
mean_rolls; sd_rolls

### Exercise 9
# Create a list with three elements: vector 1:10, string "Hello R", and TRUE
my_list <- list(vecteur = 1:10, chaine = "Hello R", booleen = TRUE)
my_list
# Access by name
my_list$vecteur
my_list$chaine
my_list$booleen

### Exercise 10
# Create a list with numeric vector c(2,4,6,8) and character vector c("a","b","c")
list2 <- list(num = c(2, 4, 6, 8), chars = c("a", "b", "c"))
# Access first element of the list
list2[[1]]         # numeric vector
# Access second element of the list
list2[[2]]         # character vector
# Access second element of the character vector
list2[[2]][2]      # "b"

### Exercise 11
# Create a list containing three numeric vectors of different lengths and compute their lengths
list3 <- list(a = 1:5, b = rnorm(10), c = c(10, 20, 30))
lapply(X = list3, FUN = length)

### Exercise 12
# Create a list with vector 1:5 and a vector of their squares
lst_num_sq <- list(nombre = 1:5, carre = (1:5)^2)

### Additional helpful utility: Exercise "sum of digits" (was in earlier)
# Function: sum of digits of a positive integer
sum_digits <- function(n) {
  if (!is.numeric(n) || n < 0 || n != floor(n)) stop("n must be a non-negative integer")
  digits <- as.integer(strsplit(as.character(abs(n)), "")[[1]])
  sum(digits)
}
# Examples
sum_digits(12345)  # 15
sum_digits(0)      # 0

###############################################################################
# Problem (simulation + interpretation)
###############################################################################

# Problem: Simulate repeated experiments of rolling a fair six-sided die

### Part 1: Create lists of simulations
# List A: 10,000 vectors of 10 rolls each -- feasible
n_sims <- 10000
n_rolls_small <- 10
list_small <- replicate(n_sims, sample(1:6, n_rolls_small, replace = TRUE), simplify = FALSE)
# Check: list_small is a list of length 10000, each element length 10
length(list_small)
length(list_small[[1]])

# List B: 10,000 vectors of 10,000 rolls each 
# Creating such a list will likely exhaust RAM on most machines. 
# Instead, we'll *not* store all vectors; we will compute the mean & sd for each simulation on the fly.
n_rolls_large <- 10000

# Naive approach (commented out) - DO NOT RUN unless you have enormous RAM:
 list_large_naive <- replicate(n_sims, sample(1:6, n_rolls_large, replace = TRUE), simplify = FALSE)

# Memory-efficient approach: compute mean & sd per simulation without storing the full vectors.
# Part 2: Function that takes a numeric vector and returns a list with "mean" and "standard-deviation"
stats_list_function <- function(x) {
  if (!is.numeric(x)) stop("x must be numeric")
  list(mean = mean(x), std = sd(x))
}
# Test
stats_list_function(1:6)

# Part 3: Apply the function to each simulation and collect results

# For the small simulations, we can apply to list_small directly
results_small <- lapply(list_small, stats_list_function)
# Extract means and sds as numeric vectors
means_small <- unlist(lapply(X = results_small, FUN = function(x) x$mean))
std_small <- unlist(lapply(X = results_small, FUN = function(x) x$std))

# For the large simulations, do not create the giant list; instead simulate each sample, compute and store stats
results_large <-
  lapply(X = 1:n_sims,
         FUN = function(x) stats_list_function(sample(1:6, n_rolls_large, replace = TRUE)))
# Extract means and sds as numeric vectors
means_large <- unlist(lapply(X = results_large, FUN = function(x) x$mean))
std_large <- unlist(lapply(X = results_large, FUN = function(x) x$std))


# Part 4: We already have the means vectors: means_small and means_large

# Quick summaries
summary(means_small)
summary(means_large)

# Part 5: Plot densities with ggplot2 to illustrate CLT / convergence of sample mean
# Install ggplot2 if needed
library(ggplot2)

df_plot <- data.frame(
  mean = c(means_small, means_large),
  sample_size = factor(rep(c(n_rolls_small, n_rolls_large), each = n_sims))
)

p <- ggplot(df_plot, aes(x = mean, color = sample_size, fill = sample_size)) +
  geom_histogram(alpha=0.5,
                 bins=1000,
                 aes(y=0.001*..density..)) +
  labs(title = "Density of sample means for die rolls",
       subtitle = paste0("Comparison for sample sizes n = ", n_rolls_small, " and n = ", n_rolls_large),
       x = "Sample mean", y = "Density", color = "Sample size", fill = "Sample size") +
  theme_minimal()
print(p)

# Save plot (optional)
# ggsave("density_sample_means.png", plot = p, width = 8, height = 5, dpi = 300)

###############################################################################
# End of script
###############################################################################

# Note on memory: creating a list of 10,000 vectors each of length 10,000 would allocate
# 100 million integers; at 8 bytes per number in R (numeric), that's ~800 MB if stored as integers? 
# Actually R's numeric uses 8 bytes -> ~800 MB, plus list overhead makes it larger.
# Many systems will struggle with that. That's why the script computes stats on the fly instead.
