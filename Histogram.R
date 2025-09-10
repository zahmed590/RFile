# Probability function in R

# Step 1 - Create the data
mean <- 20    # mean of our normal distribution
sd   <- 18    # standard deviation of our normal distribution
n    <- 10000 # number of random numbers we want to pull

# Step 2 - Generation of random numbers
rand.norm <- rnorm(n, mean = mean, sd = sd)
rand.unif <-runif(n,min=3,max=100)

# Step 3 - Make plots
par(mfrow = c(2, 2))  # splits the plotting area into 4 quadrants

# 1. Simple scatter plot of random numbers
plot(rand.norm, ylab = "Value", main = "Plot of Random Numbers")

# 2.Box plot
boxplot(rand.norm, main = "Boxplot of Random Numbers")
#3. Histogram
hist(rand.unif, main="Histogram oRandom Numbers")
