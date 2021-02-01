library(readr)
library(ggplot2)
library(ggrepel)

# S&P data from https://github.com/CNuge/kaggle-code/tree/master/stock_data
all_stocks_5yr <- read_csv("all_stocks_5yr.csv")

# remover duplicate:
all_stocks_5yr <- filter(all_stocks_5yr, Name != "GOOGL")

# convert dates to standard format:
all_stocks_5yr$date <- as.Date(all_stocks_5yr$date, "%d.%m.%y")

# add column for weekly change:
all_stocks_5yr <- group_by(all_stocks_5yr, Name) %>% 
  mutate(diff_close = close - lag(close, 5), 
         diff_log_close = log(close) - log(lag(close, 5))) %>% 
  ungroup()

# assign sets according to initial closing price:
all_stocks_5yr <- group_by(all_stocks_5yr, Name) %>% 
  mutate(close_set = round(log10(first(close)) / 0.25)) %>% 
  ungroup()

# filter so each set has no more than 3 elements:
sum_df <- group_by(all_stocks_5yr, Name, close_set) %>% 
  summarise %>% 
  group_by(close_set) %>% 
  slice(1:3)
censored_df <- filter(all_stocks_5yr, Name %in% sum_df$Name)

# assign labels for plotting:
censored_df <- mutate(censored_df, label = if_else(date == max(date) & close_set > 9, Name, NA_character_))

# small_df <- group_by(censored_df, Name) %>% 
#   filter(row_number() %% 5 == 1) %>% 
#   ungroup()

##########

# symmetrical random walk with prediction:
p1 <- 0.5
w <- 100
len <- 1e6
short_len <- 1e3
x1 <- 1e-2
y1 <- -1e-2
W1 <- sample(c(y1, x1), len, replace = TRUE, prob = c(1 - p1, p1))
S1 <- cumsum(W1) + w
Time <- seq(1, len, length = short_len)
S1 <- S1[Time]
sdf <- data.frame(price = S1, Time = Time)
# pdf("StockPriceRandomWalk.pdf", width = 4, height = 3)
par(mar = c(4,4,1,1), las = 1)
plot(price ~ Time, data = sdf, type = "l", xlim = c(0, 1.1*len), ylim = c(80, 120), 
     xlab = "time", ylab = "stock price", col = "red")
t <- seq(len, 1.1*len, length = 0.1*short_len + 1)
w <- S1[length(S1)]
m <- sapply(t - len, exp_rw, p = p1, x = x1, y = y1) + w
v <- 2 * sqrt(sapply(t - len, var_rw, p = p1, x = x1, y = y1))
tdf <- data.frame(m = m, v = v, t = t)
lines(m ~ t, data = tdf, lty = 2)
lines(m + v ~ t, data = tdf, col = "grey")
lines(m - v ~ t, data = tdf, col = "grey")
# dev.off()

##########

# short-term Amazon stock price:
# pdf("StockPriceAmazonShortTerm.pdf", width = 4*0.85, height = 3*0.85)
ggplot(filter(censored_df, Name == "AMZN", date < "2015-01-01"), aes(date, close, group = Name)) + 
  geom_line(col = "red") + 
  ylab("closing price") + 
  scale_y_continuous(limits = c(100, 550)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.5,0.2,0.2), "cm"))
# dev.off()

# long-term Amazon stock price:
# pdf("StockPriceAmazonLongTerm.pdf", width = 4*0.85, height = 3*0.85)
ggplot(filter(censored_df, Name == "AMZN"), aes(date, close, group = Name)) + 
  geom_line(col = "red") + 
  ylab("closing price") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.5,0.2,0.2), "cm"))
# dev.off()

# many stock prices (linear scale):
# pdf("StockPricesLinear.pdf", width = 4*0.85, height = 3*0.85)
ggplot(censored_df, aes(date, close, group = Name)) + 
  geom_line(col = "red") + 
  geom_label_repel(aes(label = label),
                   size = 2, 
                   box.padding = 0.1,
                   label.padding = 0.1,
                   nudge_x = 500,
                   na.rm = TRUE) + 
  ylab("closing price") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.5,0.2,0.2), "cm"))
# dev.off()

# many stock prices (log scale):
# pdf("StockPricesLog.pdf", width = 4*0.85, height = 3*0.85)
ggplot(censored_df, aes(date, close, group = Name)) + 
  geom_line(col = "red") + 
  scale_y_log10() + 
  geom_label_repel(aes(label = label),
                   size = 2, 
                   box.padding = 0,
                   label.padding = 0.1,
                   nudge_x = 500,
                   na.rm = TRUE) + 
  ylab("closing price") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.5,0.2,0.2), "cm"))
# dev.off()

# absolute weekly change in closing price versus closing price (log-log scale):
# pdf("StockPriceChanges.pdf", width = 4*0.85, height = 3*0.85)
ggplot(filter(censored_df, close > 10), aes(close, abs(diff_close))) + 
  geom_point(alpha = 0.01) + 
  geom_smooth() + 
  xlab("closing price") + 
  ylab("absolute change in closing price") + 
  scale_y_log10() + 
  scale_x_log10() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.5,0.2,0.2), "cm"))
# dev.off()

# absolute weekly change in closing price versus closing price (log-log scale):
# pdf("StockPriceChangesLog.pdf", width = 4*0.85, height = 3*0.85)
ggplot(filter(censored_df, close > 10), aes(close, abs(diff_log_close))) + 
  geom_point(alpha = 0.01) + 
  geom_smooth() + 
  xlab("closing price") + 
  ylab("abs. change in log closing price") + 
  scale_y_log10() + 
  scale_x_log10() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5,0.5,0.2,0.2), "cm"))
# dev.off()

##########

# geometric random walks:
p1 <- 0.5
w <- 100
u <- 1.001
d <- 2 - 1.001
len <- 1e6
short_len <- 1e3
Time <- seq(1, len, length = short_len)
sdf <- data.frame()
for(i in 1:5) {
  W1 <- sample(c(0, 1), len, replace = TRUE, prob = c(1 - p1, p1))
  W1 <- cumsum(W1)
  S1 <- w * u^W1 * d^(1:len - W1)
  S1_alt <- log(w) + log(u) * W1 + log(d) * (1:len - W1)
  S1 <- S1[Time]
  S1_alt <- S1_alt[Time]
  W1 <- W1[Time]
  sdf <- rbind(sdf, data.frame(Time = Time, 
                               Wn = W1, 
                               price = S1, 
                               price_alt = S1_alt, 
                               index = as.character(i)))
}
# pdf("StockPriceGeoRandomWalkLinearScale.pdf", width = 4*0.85, height = 3*0.85)
ggplot(sdf, aes(Time, price, group = index, col = index)) + 
  geom_line() + 
  xlab("time") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  theme(plot.margin = unit(c(0.5,0.5,0.2,0.2), "cm"))
# dev.off()

# pdf("StockPriceGeoRandomWalkLogScale.pdf", width = 4*0.85, height = 3*0.85)
ggplot(sdf, aes(Time, price, group = index, col = index)) + 
  geom_line() + 
  xlab("time") + 
  scale_y_log10() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  theme(plot.margin = unit(c(0.5,0.5,0.2,0.2), "cm"))
# dev.off()

# long-term Amazon stock price with forecast:
start_date <- min(filter(censored_df, Name == "AMZN")$date)
end_date <- max(filter(censored_df, Name == "AMZN")$date)
start_price <- filter(censored_df, Name == "AMZN", date == start_date)$close
end_price <- filter(censored_df, Name == "AMZN", date == end_date)$close

rate <- (log(end_price) - log(start_price)) / as.numeric((end_date - start_date))
ft <- seq(start_date, end_date + 100, length = 1e3)
fdf <- data.frame(time = ft, forecast = start_price * exp(rate * (as.numeric(ft - start_date))))



