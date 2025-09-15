# 1. Load required libraries
library(fitdistrplus)  # for distribution fitting (if needed)
library(e1071)         # for skewness calculation
library(ggplot2)       # for plotting
setwd("D:/Elma")
a <- read.csv("250909_modelfit.csv")
flow_data <- a[, 1]   # numeric vector from the first column
# 4. Calculate the number of bins using Doane’s formula (with skewness from e1071)
n <- length(flow_data)
skew_val <- skewness(flow_data)  # compute skewness using e1071
sigma_g1 <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))  # standard error of skewness
doane_bins <- ceiling(1 + log2(n) + log2(1 + abs(skew_val) / sigma_g1))
# (Using abs(skew_val) to ensure the argument of the log is positive)

# 5. Plot a histogram using ggplot2 with the computed number of bins and styling
ggplot(data.frame(flow_data), aes(x = flow_data)) +
  geom_histogram(bins = doane_bins, fill = "pink", color = "black") +
  labs(x = "Flow Rate", y = "Frequency", title = "Histogram using Doane's Formula") +
  theme_minimal()