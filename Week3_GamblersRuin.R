p <- seq(0, 1, length = 1000)
u_w <- function(p, w, A) {
  if(p == 0.5) return(w / A)
  return((1 - ((1 - p) / p)^w) / (1 - ((1 - p) / p)^A))
}

# mean and variance of binomial distribution:
exp_rw <- function(n, p, x, y) n * (p * x + (1 - p) * y)
var_rw <- function(p, n, x, y) n * p * (1 - p) * (x - y)^2

p1 <- 0.55
w <- 10
ruin_count <- 0
end_count <- 0
W1 <- sample(c(-1, 1), 200, replace = TRUE, prob = c(1 - p1, p1))
S1 <- cumsum(W1) + 10
if(min(S1) <= 0) ruin_count <- ruin_count + 1
if(S1[length(S1)] <= 0) end_count <- end_count + 1
# pdf("GamblersRuin.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(S1, type = "l", ylim = c(-10, 60), xlab = "Time", ylab = "Wealth", col = rgb(0,0,0,0.02))
for(i in 1:999) {
  W1 <- sample(c(-1, 1), 200, replace = TRUE, prob = c(1 - p1, p1))
  S1 <- cumsum(W1) + 10
  if(min(S1) <= 0) ruin_count <- ruin_count + 1
  if(S1[length(S1)] <= 0) end_count <- end_count + 1
  lines(S1, col = rgb(0,0,0,0.02))
}
t <- 0:200
m <- sapply(t, exp_rw, p = p1, x = 1, y = -1) + w
v <- 2 * sqrt(sapply(t, var_rw, p = p1, x = 1, y = -1))
lines(m, col = "gold")
lines(m + v, col = "magenta")
lines(m - v, col = "magenta")
abline(h = 0, lty = 2)
# dev.off()

# pdf("ProbReachingA.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
res <- sapply(p, u_w, w = 3, A = 20)
plot(res ~ p, type = "l", xlab = "p (probability of winning each round)", ylab = "u(w)", col = "gold", lwd = 2, las = 1)
abline(v = 0.5, col = "black", lty = 2, lwd = 2)
res <- sapply(p, u_w, w = 10, A = 20)
lines(res ~ p, col = "blue", lwd = 2)
res <- sapply(p, u_w, w = 1, A = 20)
lines(res ~ p, col = "red", lwd = 2)
# dev.off()

# pdf("ProbReachingA2.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
w <- 0:20
res <- sapply(w, u_w, p = 0.5, A = 20)
plot(res ~ w, type = "l", xlab = "w (initial amount of money)", ylab = "u(w)", col = "gold", lwd = 2, ylim = c(0, 1), las = 1)
res <- sapply(w, u_w, p = 0.45, A = 20)
lines(res ~ w, col = "red", lwd = 2)
res <- sapply(w, u_w, p = 0.55, A = 20)
lines(res ~ w, col = "blue", lwd = 2)
# dev.off()

# pdf("ProbReachingA3.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
A <- 10:50
res <- sapply(A, u_w, p = 0.5, w = 10)
plot(res ~ A, type = "l", xlab = "A (target amount of money)", ylab = "u(w)", 
     col = "gold", lwd = 2, xlim = c(0, 50), ylim = c(0, 1), las = 1)
res <- sapply(A, u_w, p = 0.45, w = 10)
lines(res ~ A, col = "red", lwd = 2)
res <- sapply(A, u_w, p = 0.55, w = 10)
lines(res ~ A, col = "blue", lwd = 2)
# dev.off()

v_w <- function(p, w, A) {
  if(p == 0.5) return(w * (A - w))
  top <- -A * (1 - ((1 - p) / p)^w) + w * (1 - ((1 - p) / p)^A)
  bot <- (1 - ((1 - p) / p)^A) * (1 - 2 * p)
  return(top / bot)
}

# pdf("DurationOfGame.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
res <- sapply(p, v_w, w = 3, A = 20)
plot(res ~ p, type = "l", xlab = "p (probability of winning each round)", ylab = "v(w)", 
     col = "gold", lwd = 2, ylim = c(0, 100), las = 1)
abline(v = 0.5, col = "black", lty = 2, lwd = 2)
res <- sapply(p, v_w, w = 10, A = 20)
lines(res ~ p, col = "blue", lwd = 2)
res <- sapply(p, v_w, w = 1, A = 20)
lines(res ~ p, col = "red", lwd = 2)
# dev.off()

# pdf("DurationOfGame2.pdf", width = 4, height = 3)
w <- 0:20
par(mar = c(4,4,1,1))
res <- sapply(w, v_w, p = 0.5, A = 20)
plot(res ~ w, type = "l", xlab = "w (initial amount of money)", ylab = "v(w)", 
     col = "gold", lwd = 2, ylim = c(0, 100), las = 1)
res <- sapply(w, v_w, p = 0.45, A = 20)
lines(res ~ w, col = "red", lwd = 2)
res <- sapply(w, v_w, p = 0.55, A = 20)
lines(res ~ w, col = "blue", lwd = 2)
# dev.off()

# pdf("DurationOfGame3.pdf", width = 4, height = 3)
A <- 10:50
par(mar = c(4,4,1,1))
res <- sapply(A, v_w, p = 0.5, w = 10)
plot(res ~ A, type = "l", xlab = "A (target amount of money)", ylab = "v(w)", 
     col = "gold", lwd = 2, xlim = c(0, 50), ylim = c(0, 400), las = 1)
res <- sapply(A, v_w, p = 0.45, w = 10)
lines(res ~ A, col = "red", lwd = 2)
res <- sapply(A, v_w, p = 0.55, w = 10)
lines(res ~ A, col = "blue", lwd = 2)
# dev.off()

len_vec <- vector()
W1 <- sample(c(-1, 1), 1e5, replace = TRUE, prob = c(1 - p1, p1))
S1 <- cumsum(W1) + 10
t1 <- min(which(S1 == 0))
t2 <- min(which(S1 == 20))
len_vec <- c(len_vec, min(t1, t2))
for(i in 1:9999) {
  W1 <- sample(c(-1, 1), 1e5, replace = TRUE, prob = c(1 - p1, p1))
  S1 <- cumsum(W1) + 10
  t1 <- min(which(S1 == 0))
  t2 <- min(which(S1 == 20))
  len_vec <- c(len_vec, min(t1, t2))
}
# pdf("DurationHistogram.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
hist(len_vec, breaks = 80, xlab = "v(w)", ylab = "density", 
     main = "", freq = FALSE, xlim = c(0, 400))
abline(v = mean(len_vec), col = "magenta")
abline(v = v_w(0.55, 10, 20), lty = 2)
# dev.off()

len_vec2 <- vector()
W1 <- sample(c(-1, 1), 1e5, replace = TRUE)
S1 <- cumsum(W1) + 10
t1 <- min(which(S1 == 0))
t2 <- min(which(S1 == 100))
len_vec2 <- c(len_vec2, min(t1, t2))
for(i in 1:9999) {
  W1 <- sample(c(-1, 1), 1e5, replace = TRUE)
  S1 <- cumsum(W1) + 10
  t1 <- min(which(S1 == 0))
  t2 <- min(which(S1 == 100))
  len_vec2 <- c(len_vec2, min(t1, t2))
}
# pdf("DurationHistogram2.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
hist(len_vec2, breaks = 400, xlab = "v(w)", ylab = "density", 
     main = "", freq = FALSE, xlim = c(0, 4e3))
abline(v = mean(len_vec2), col = "magenta")
abline(v = v_w(0.5, 10, 100), lty = 2)
# dev.off()

