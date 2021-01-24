library(ggplot2)
library(dplyr)

# mean and variance of binomial distribution:
exp_rw <- function(n, p, x, y) n * (p * x + (1 - p) * y)
var_rw <- function(p, n, x, y) n * p * (1 - p) * (x - y)^2

# random walk parameters:
w_vec <- seq(floor(exp_Wn - 5 * sd_Wn), ceiling(exp_Wn + 5 * sd_Wn), length = 1e3)
p1 <- 0.8
x <- 1
y <- -5
n1 <- 20

# mean and variance of random walk:
exp_Wn <- exp_rw(p = p1, n = n1, x = x, y = y)
sd_Wn <- sqrt(var_rw(p = p1, n = n1, x = x, y = y))

# PMF of random walk:
rw_pmf <- function(p, x, y, w_n, n) {
  k <- (w_n - n * y) / (x - y)
  if(k != round(k)) return(NA)
  if(choose(n, k) == 0) return(NA)
  return(choose(n, k) * p^k * (1 - p)^(n - k))
}

# data frame containing PMF of random walk for given parameters:
w_n_vec <- floor(exp_Wn - 5 * sd_Wn):ceiling(exp_Wn + 5 * sd_Wn)
pmf_vec <- sapply(w_n_vec, rw_pmf, p = p1, x = x, y = y, n = n1)
tab0 <- as.data.frame(cbind(w_n_vec, pmf_vec))
colnames(tab0)[1] <- "w"

# PDF of normal distribution for given parameters:
norm_approx <- function(exp_Wn, sd_Wn, x, y, w_n, p1, n1) {
  exp_Wn <- exp_rw(p = p1, n = n1, x = x, y = y)
  sd_Wn <- sqrt(var_rw(p = p1, n = n1, x = x, y = y))
  p_vec1 <- (w_n + (x-y)/2 - exp_Wn) / sd_Wn
  p_vec2 <- (w_n - (x-y)/2 - exp_Wn) / sd_Wn
  approx1 <- sapply(p_vec1, pnorm) - sapply(p_vec2, pnorm)
  return(approx1)
}

# data frame containing PDF of normal distribution for given parameters:
approx1 <- sapply(w_n_vec, norm_approx, exp_Wn = exp_Wn, sd_Wn = sd_Wn, x = x, y = y, p = p1, n = n1)
tab2 <- as.data.frame(cbind(w_n_vec, approx1))
colnames(tab2)[1] <- "w"

# merge data frames:
df <- merge(tab0, tab2, all = TRUE)

# plot binomial distribution and approximation(s):
# pdf("RandomWalkNormalApprox.pdf", width = 5, height = 3)
ggplot(df, aes(x = w, y = pmf_vec)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "red") + 
  geom_line(aes(x = w, y = approx1)) + 
  theme_classic() + 
  scale_x_continuous(breaks = 6*(-10:4) - 4) + 
  labs(x = expression(w[20]), y = "probability distribution")
# dev.off()

rw_pmf(p1, x, y, -200, 1000)
norm_approx(exp_Wn, sd_Wn, x, y, -200, p1, 1000)

#########

# uniform:
n_draws <- 1e5
get_data <- function(n) {
  mf <- function(x, n) sum(runif(n))
  dist1 <- (sapply(rep(1, n_draws), mf, n = n) - n / 2) / sqrt(n / 12)
  return(cbind(n, dist1))
}
dist1 <- get_data(1)
dist2 <- get_data(2)
dist3 <- get_data(10)
dist_n <- cbind(n = Inf, y = rnorm(n_draws))
tab1 <- as.data.frame(rbind(dist1, dist2, dist3, dist_n))
tab1$n <- as.factor(tab1$n)
levels(tab1$n) <- c("n = 1", "n = 2", "n = 10", "normal")

# pdf("CentralLimitUniform.pdf", width = 4, height = 3)
ggplot(tab1, aes(x = dist1, group = n, col = n, linetype = n)) + 
  geom_density() + 
  labs(x = expression(X[1] + ... + X[n])) + 
  theme_classic() + 
  scale_color_manual(values = c("red", "blue", "magenta", "black")) + 
  scale_linetype_manual(values = c("solid","solid","solid","dashed")) + 
  theme(legend.title = element_blank())
# dev.off()

