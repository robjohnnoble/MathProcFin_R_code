library(ggplot2)
library(dplyr)

# mean and variance of binomial distribution:
exp_rw <- function(n, p, x, y) n * (p * x + (1 - p) * y)
var_rw <- function(p, n, x, y) n * p * (1 - p) * (x - y)^2

# random walk parameters:
p1 <- 0.8
x <- 1
y <- -5
n1 <- 20

# mean and variance of random walk:
exp_Wn <- exp_rw(p = p1, n = n1, x = x, y = y)
sd_Wn <- sqrt(var_rw(p = p1, n = n1, x = x, y = y))

w_vec <- seq(floor(exp_Wn - 5 * sd_Wn), ceiling(exp_Wn + 5 * sd_Wn), length = 1e3)

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
norm_approx <- function(x, y, w_n, p1, n1, correction = 1/2) {
  exp_Wn <- exp_rw(p = p1, n = n1, x = x, y = y)
  sd_Wn <- sqrt(var_rw(p = p1, n = n1, x = x, y = y))
  p_vec1 <- (w_n + (x-y)*correction - exp_Wn) / sd_Wn
  p_vec2 <- (w_n - (x-y)*correction - exp_Wn) / sd_Wn
  approx1 <- sapply(p_vec1, pnorm) - sapply(p_vec2, pnorm)
  return(approx1)
}

# data frame containing PDF of normal distribution for given parameters:
approx1 <- sapply(w_n_vec, norm_approx, x = x, y = y, p = p1, n = n1)
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
norm_approx(x, y, -200, p1, 1000)

#########
## illustration of Central Limit Theorem using uniform distribution

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

#########
## Illustration of continuity correction

p2 <- 0.5
n2 <- 20
x2 <- 3
y2 <- -1
mu <- exp_rw(p = p2, n = n2, x = x2, y = y2)
sd <- sqrt(var_rw(p = p2, n = n2, x = x2, y = y2))

xx <- seq(n2 * y2, n2 * x2, length = 1e3)

xn <- seq(n2 * y2, n2 * x2, by = x2 - y2)
bvec <- sapply(xn, rw_pmf, p = p2, x = x2, y = y2, n = n2)
cbvec <- cumsum(bvec)
xn_s <- xn - 1e-6
xn_a <- sort(c(xn, xn_s))
cbvec_a <- rep(cbvec, each = 2)
cbvec_a <- c(0, cbvec_a[-length(cbvec_a)])

nvec <- sapply(xx, pnorm, mean = mu, sd = sd)

# pdf("ContinuityCorrection1.pdf", width = 5, height = 3.5)
par(mar = c(4,4,1,1))
plot(nvec ~ xx, xlim = c(0, 40), xaxt = "n", 
     ylim = c(0, 1), type = "l", col = "red", lwd = 2, 
     xlab = expression(w[20]), ylab = expression(P(W[20] <= w[20])))
lines(cbvec_a ~ xn_a, lwd = 2)
axis(1, at = 4 * 0:10)
points(cbvec ~ xn)
# dev.off()

wn <- 24

stop_ind_bin <- min(which(xn_a >= wn))
stop_ind_norm_bad1 <- min(which(xx >= wn))
stop_ind_norm_bad2 <- min(which(xx >= wn + x2 - y2))

# pdf("ContinuityCorrection2.pdf", width = 5, height = 3.5)
par(mar = c(4,4,1,1))
plot(nvec ~ xx, xlim = c(0, 40), xaxt = "n", 
     ylim = c(0, 1), type = "l", col = "red", lwd = 2, 
     xlab = expression(w[20]), ylab = expression(P(W[20] <= w[20])))
lines(cbvec_a ~ xn_a, lwd = 2)
points(cbvec ~ xn)
abline(v = wn, lty = 2, col = "red")
abline(h = nvec[stop_ind_norm_bad1], lty = 2, col = "red")
#abline(v = wn + x2 - y2, lty = 2, col = "magenta")
#abline(h = nvec[stop_ind_norm_bad2], lty = 2, col = "magenta")
abline(v = wn, lty = 3)
abline(h = cbvec_a[stop_ind_bin], lty = 3)
axis(1, at = 4 * 0:10)
# dev.off()

stop_ind_norm <- min(which(xx >= wn + (x2 - y2) / 2))
stop_ind_norm_alt <- min(which(xx >= wn + x2))

# pdf("ContinuityCorrection3.pdf", width = 5, height = 3.5)
par(mar = c(4,4,1,1))
plot(nvec ~ xx, xlim = c(0, 40), xaxt = "n", 
     ylim = c(0, 1), type = "l", col = "red", lwd = 2, 
     xlab = expression(w[20]), ylab = expression(P(W[20] <= w[20])))
lines(cbvec_a ~ xn_a, lwd = 2)
points(cbvec ~ xn)
abline(v = wn + (x2 - y2) / 2, lty = 2, col = "red")
abline(h = nvec[stop_ind_norm], lty = 2, col = "red")
# abline(v = wn + x2, lty = 2, col = "blue")
# abline(h = nvec[stop_ind_norm_alt], lty = 2, col = "blue")
abline(v = wn, lty = 3)
abline(h = cbvec_a[stop_ind_bin], lty = 3)
axis(1, at = 4 * 0:10)
# dev.off()

nvec_pmf <- sapply(xx, norm_approx, x = x2, y = y2, p = p2, n = n2)

plot(nvec_pmf ~ xx, 
     ylim = c(0, 0.2), type = "l", col = "red", lwd = 2, 
     xlab = expression(w[20]), ylab = expression(P(W[20] == w[20])))
points(bvec ~ xn)





