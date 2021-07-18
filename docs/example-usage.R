library(lnormimp)

set.seed(1234)

data <- rlnorm(400, meanlog = 2, sdlog = 1)

plot(density(data), col = "blue", lty = "dashed", main = "Probability-density")

lower_cutoff <- 1
upper_cutoff <- 15

abline(v = lower_cutoff, col = "darkgray")
abline(v = upper_cutoff, col = "darkgray")

below_lower <- data < lower_cutoff
above_upper <- data > upper_cutoff

n_below <- sum(below_lower)
n_above <- sum(above_upper)

data_censored <- data[!(below_lower | above_upper)]

data.frame(n = length(data),
           n_below = n_below,
           n_above = n_above,
           n_censored = length(data_censored))

lines(density(data_censored), col = "red", lty = "dotted")

data_imputed <- lnormimp(
  data_censored,
  censn = c(n_below, n_above),
  cutoff = c(lower_cutoff, upper_cutoff)
)

lines(density(data_imputed), col = "darkgreen")

ks.test(data, data_imputed)$statistic
ks.test(data, data_censored)$statistic
