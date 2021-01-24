# variance of random walk:
var_rw <- function(p, n, x, y) n * p * (1 - p) * (x - y)^2
# expectation of random walk:
exp_rw <- function(n, p, x, y) n * (p * x + (1 - p) * y)
# PMF of random walk:
rw_pmf <- function(p, x, y, w_n, n) {
  k <- (w_n - n * y) / (x - y)
  if(k != round(k)) return(NA)
  if(choose(n, k) == 0) return(NA)
  return(choose(n, k) * p^k * (1 - p)^(n - k))
}

# plot trajectories of a symmetric random walk (black) 
# and an asymmetric random walk (red):
W1 <- sample(c(-1, 1), 100, replace = TRUE)
S1 <- cumsum(W1)
W2 <- sample(c(-1, 1), 100, replace = TRUE, prob = c(0.2, 0.8))
S2 <- cumsum(W2)
# pdf("RandomWalk.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(S1, type = "l", ylim = c(-20, 80), xlab = "Time", ylab = "Position")
lines(S2, col = "red")
# dev.off()

# plot five trajectories of a symmetric random walk:
W1 <- sample(c(-1, 1), 100, replace = TRUE)
S1 <- cumsum(W1)
# pdf("RandomWalk2.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(S1, type = "l", ylim = c(-20, 20), xlab = "Time", ylab = "Position")
cols <- c("red", "blue", "gold", "green3")
for(i in 1:4) {
  W1 <- sample(c(-1, 1), 100, replace = TRUE)
  S1 <- cumsum(W1)
  lines(S1, col = cols[i])
}
# dev.off()

# plot 1,000 trajectories of a symmetric random walk:
W1 <- sample(c(-1, 1), 100, replace = TRUE)
S1 <- cumsum(W1)
# pdf("RandomWalkMany.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(S1, type = "l", ylim = c(-40, 40), xlab = "Time", ylab = "Position", col = rgb(0,0,0,0.02))
for(i in 1:999) {
  W1 <- sample(c(-1, 1), 100, replace = TRUE)
  S1 <- cumsum(W1)
  lines(S1, col = rgb(0,0,0,0.02))
}
# dev.off()

# plot 1,000 trajectories of a symmetric random walk 
# with curves showing predicted mean and standard deviation:
W1 <- sample(c(-1, 1), 100, replace = TRUE)
S1 <- cumsum(W1)
dist1 <- S1[100]
# pdf("RandomWalkManyWithStdev.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(S1, type = "l", ylim = c(-40, 40), xlab = "Time", ylab = "Position", col = rgb(0,0,0,0.02))
for(i in 1:999) {
  W1 <- sample(c(-1, 1), 100, replace = TRUE)
  S1 <- cumsum(W1)
  lines(S1, col = rgb(0,0,0,0.02))
}
t <- 1:100
v <- 2 * sqrt(sapply(t, var_rw, p = 0.5, x = 1, y = -1))
lines(pmin(t, 0), col = "gold")
lines(v, col = "magenta")
lines(-v, col = "magenta")
# dev.off()

# plot 1,000 trajectories of an asymmetric random walk 
# with curves showing predicted mean and standard deviation:
p1 <- 0.8
W1 <- sample(c(-1, 1), 100, replace = TRUE, prob = c(1 - p1, p1))
S1 <- cumsum(W1)
# pdf("RandomWalkManyWithStdevAsymmetric.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(S1, type = "l", ylim = c(-20, 100), xlab = "Time", ylab = "Position", col = rgb(0,0,0,0.02))
for(i in 1:999) {
  W1 <- sample(c(-1, 1), 100, replace = TRUE, prob = c(1 - p1, p1))
  S1 <- cumsum(W1)
  lines(S1, col = rgb(0,0,0,0.02))
}
t <- 1:100
m <- sapply(t, exp_rw, p = p1, x = 1, y = -1)
v <- 2 * sqrt(sapply(t, var_rw, p = p1, x = 1, y = -1))
lines(m, col = "gold")
lines(m + v, col = "magenta")
lines(m - v, col = "magenta")
abline(h = 0, lty = 2)
# dev.off()

# plot 1,000 trajectories each of three asymmetric random walks 
# with curves showing predicted means and standard deviations:
pdf("RandomWalkManyWithStdevAsymmetric2.pdf", width = 4, height = 3)
par(mar = c(4,4,1,3.2))
W1 <- sample(c(-1, 1), 100, replace = TRUE, prob = c(1 - p1, p1))
S1 <- cumsum(W1)
# plot(S1, type = "l", ylim = c(-20, 100), xlab = "Time", ylab = "Position", col = rgb(0,0,0,0.02))
for(p1 in c(0.6, 0.8, 0.95)) {
  W1 <- sample(c(-1, 1), 100, replace = TRUE, prob = c(1 - p1, p1))
  S1 <- cumsum(W1)
  for(i in 1:1000) {
    W1 <- sample(c(-1, 1), 100, replace = TRUE, prob = c(1 - p1, p1))
    S1 <- cumsum(W1)
    lines(S1, col = rgb(0,0,0,0.02))
  }
}
t <- 1:100
for(p1 in c(0.6, 0.8, 0.95)) {
  m <- sapply(t, exp_rw, p = p1, x = 1, y = -1)
  v <- 2 * sqrt(sapply(t, var_rw, p = p1, x = 1, y = -1))
  lines(m, col = "gold")
  lines(m + v, col = "magenta")
  lines(m - v, col = "magenta")
}
abline(h = 0, lty = 2)
mtext(" p = 0.6", 4, las = 2, at = 100 * (0.6 - (1 - 0.6)))
mtext(" p = 0.8", 4, las = 2, at = 100 * (0.8 - (1 - 0.8)))
mtext(" p = 0.95", 4, las = 2, at = 100 * (0.95 - (1 - 0.95)))
# dev.off()

# plot PMF of a random walk:
n <- 10
p <- 0.5
x <- 2
y <- -1
w_n_vec <- (n * y):(n * x)
# pdf("RandomWalk_PMF.pdf", width = 4, height = 3)
pmf_vec <- sapply(w_n_vec, rw_pmf, p = p, x = x, y = y, n = n)
plot(pmf_vec ~ w_n_vec, 
     xlab = expression(w[n]), 
     ylab = expression(paste("P(", W[n] ," = ", w[n], ")")), 
     ylim = c(0, max(pmf_vec, na.rm = TRUE)))
# dev.off()





