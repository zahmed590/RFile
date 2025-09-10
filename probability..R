# Load package
library(fitdistrplus)

# Set working directory
setwd("D:/Elma")

# --- 1) Read in your model fit CSV ---
a <- read.csv("250909_modelfit.csv")
data <- a[,1]

# Fit gamma distribution (method of moments)
norm.a <- fitdist(data, "gamma", method = "mme")
norm.a


# --- 2) Soil moisture example ---
sm <- read.csv("soilmoisturedata.csv")

# Check column names to be sure
names(sm)

# Extract soil moisture for Mason station
msm <- sm$mason..mm.

summary(msm)

# Fit gamma distribution (MLE)
gm <- fitdist(msm, "gamma", method = "mle")
gm$aic

# Fit Weibull distribution (MLE)
wb <- fitdist(msm, "weibull", method = "mle")
wb$aic
soln <-c(norm.a$aic,gm$aic,wb$aic)
soln