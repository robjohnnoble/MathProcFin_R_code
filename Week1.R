# Illustration of the Poisson limit theorem
# (also known as the law of rare events):
# the binomial distribution resembles the Poisson distribution
# when n is very large and p is very small:

n <- 1e5 # very large number of trials
p1 <- 100 / n # very small probability of success per trial
k <- 1:n # vector containing all possible values of k (the number of successes)
b_vec1 <- dbinom(k, n, prob = p1) # calculate values of binomial distribution PMF
p_vec1 <- dpois(k, n * p1) # calculate values of Poisson distribution PMF

#pdf("Binomial_Poisson.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(b_vec1 ~ k, col = "red", xlim = c(0, 2*n*p1), ylab = "P(X = k)") # plot binomial distribution
lines(p_vec1 ~ k) # plot Poisson distribution
#dev.off()

# The Central limit theorem (covered in Week 2)
# further implies that the binomial distribution resembles the
# normal distribution when n is large:
n_vec1 <- dnorm(k, n * p1, sqrt(n * p1 * (1 - p1))) # calculate values of normal distribution PDF
lines(n_vec1 ~ k, col = "green3") # plot normal distribution

#####

# When p is not very small, the Poisson distribution does not approximate
# the binomial distribution. But if n is large then the normal distribution
# does approximate the binomial distribution:

n <- 1e5 # very large number of trials
p2 <- 0.5 # intermediate probability of success per trial
k <- (0:1e4) * n / 1e4 # vector containing possible values of k (the number of successes)
b_vec2 <- dbinom(k, n, p2)  # calculate values of binomial distribution PMF
p_vec2 <- dpois(k, n * p2) # calculate values of Poisson distribution PMF
n_vec2 <- dnorm(k, n * p2, sqrt(n * p2 * (1 - p2))) # calculate values of normal distribution PDF

#pdf("Binomial_Normal_Poisson.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(b_vec2 ~ k, col = "red", xlim = c(4.8e4, 5.2e4), ylab = "P(X = k)") # plot binomial distribution
lines(p_vec2 ~ k) # plot Poisson distribution
lines(n_vec2 ~ k, col = "green3") # plot normal distribution
#dev.off()

#####

# Illustration that variables can be uncorrelated
# even if they are not independent:

x <- seq(0, pi, length = 10) # x is a sequence of values between zero and pi
y <- sin(x) # y = sin(x)
cor(y, x) # correlation between x and y is zero
# (R calculates the value as extremely close to zero due to numerical errors)

#pdf("Uncorrelated.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1))
plot(y ~ x) # plot y versus x
abline(v = pi / 2, lty = 2) # vertical line at the midpoint, which is a line of symmetry
#dev.off()


