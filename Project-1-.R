# ============================
# CE 5326 Project 1 – Frequency Analysis
# Q1–Q9: Full Analysis (1929–2016 and 1929–2022)
# ============================

# ---- 0) Packages ----
install.packages(c("dataRetrieval","dplyr","lubridate","ggplot2",
                   "fitdistrplus","extRemes","gridExtra"))
library(dataRetrieval)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fitdistrplus)
library(extRemes)
library(gridExtra)

# ---- 1) Download and prepare data ----
site <- "08069000"   # Cypress Creek nr Westfield, TX
peaks_raw <- readNWISpeak(site)

peaks <- peaks_raw %>%
  transmute(date = as.Date(peak_dt),
            Q_cfs = as.numeric(peak_va)) %>%
  filter(!is.na(date), !is.na(Q_cfs)) %>%
  mutate(HY = ifelse(month(date) >= 10, year(date)+1, year(date)))

annual_max <- peaks %>%
  group_by(HY) %>%
  summarise(Qmax = max(Q_cfs, na.rm=TRUE), .groups="drop") %>%
  arrange(HY)

# ---- Skewness helper ----
skewness <- function(x){
  n <- length(x); m <- mean(x); s <- sd(x)
  g1 <- (n/((n-1)*(n-2))) * sum(((x - m)/s)^3)
  return(g1)
}

# ============================
# Q1–Q5: 1929–2016 Analysis
# ============================
am_29_16 <- annual_max %>% filter(HY >= 1929, HY <= 2016)
x1 <- am_29_16$Qmax; n1 <- length(x1)

# Histogram
g1 <- skewness(x1)
se_g1 <- sqrt(6*(n1-2)/((n1+1)*(n1+3)))
k_doane1 <- 1 + log2(n1) + log2(1 + abs(g1)/se_g1)

p_hist <- ggplot(am_29_16, aes(x=Qmax)) +
  geom_histogram(bins=round(k_doane1), fill="skyblue", color="black", alpha=1) +
  labs(title="Histogram for 1929–2016 (Doane bins)",
       x="Peak Discharge (cfs)", y="Count") +
  theme_bw()

# ECDF
x1_sorted <- sort(x1)
F_gring <- (1:n1 - 0.44)/(n1 + 0.12)

p_ecdf <- ggplot(data.frame(x1_sorted, F_gring),
                 aes(x=x1_sorted, y=F_gring)) +
  geom_point(color="black") +
  labs(title="ECDF (Gringorten) for 1929–2016",
       x="Flow (cfs)", y="Non-exceedance Probability") +
  theme_bw()

# Show side by side
grid.arrange(p_hist, p_ecdf, ncol=2)

# Distribution fitting
fit_lnorm <- fitdist(x1, "lnorm", method="mle")
fit_gumbel <- fevd(x1, type="Gumbel", method="MLE")
fit_gev    <- fevd(x1, type="GEV", method="MLE")

cat("AIC (1929–2016):\n")
cat("  Lognormal =", fit_lnorm$aic, "\n")
cat("  Gumbel    =", fit_gumbel$AIC, "\n")
cat("  GEV       =", fit_gev$AIC, "\n")

# Return levels
cat("\nReturn levels (1929–2016, Gumbel):\n")
print(return.level(fit_gumbel, return.period = c(10, 50, 100, 500)))

# Harvey exceedance (2017)
Q2017 <- annual_max %>% filter(HY == 2017) %>% pull(Qmax)
params <- fit_gumbel$results$par
mu <- params["location"]; beta <- params["scale"]

F2017 <- exp(-exp(-(Q2017 - mu)/beta))
p_exc <- 1 - F2017; T2017 <- 1/p_exc

cat("\nHarvey 2017 peak =", Q2017, "cfs\n")
cat("Exceedance probability (1929–2016) =", p_exc, "\n")
cat("Return period (years, 1929–2016) ≈", T2017, "\n")

# ============================
# Q6–Q9: 1929–2022 Analysis
# ============================
am_29_22 <- annual_max %>% filter(HY >= 1929, HY <= 2022)
x2 <- am_29_22$Qmax; n2 <- length(x2)

# Histogram
g2 <- skewness(x2)
se_g2 <- sqrt(6*(n2-2)/((n2+1)*(n2+3)))
k_doane2 <- 1 + log2(n2) + log2(1 + abs(g2)/se_g2)

p_hist2 <- ggplot(am_29_22, aes(x=Qmax)) +
  geom_histogram(bins=round(k_doane2), fill="gold", color="black", alpha=1) +
  labs(title="Histogram for 1929–2022 (Doane bins)",
       x="Peak Discharge (cfs)", y="Count") +
  theme_bw()

# ECDF
x2_sorted <- sort(x2)
F_gring2 <- (1:n2 - 0.44)/(n2 + 0.12)

p_ecdf2 <- ggplot(data.frame(x2_sorted, F_gring2),
                  aes(x=x2_sorted, y=F_gring2)) +
  geom_point(color="blue") +
  labs(title="ECDF (Gringorten) for 1929–2022",
       x="Flow (cfs)", y="Non-exceedance Probability") +
  theme_bw()

# Show side by side
grid.arrange(p_hist2, p_ecdf2, ncol=2)

# Distribution fitting
fit_lnorm2 <- fitdist(x2, "lnorm", method="mle")
fit_gumbel2 <- fevd(x2, type="Gumbel", method="MLE")
fit_gev2    <- fevd(x2, type="GEV", method="MLE")

cat("\nAIC (1929–2022):\n")
cat("  Lognormal =", fit_lnorm2$aic, "\n")
cat("  Gumbel    =", fit_gumbel2$AIC, "\n")
cat("  GEV       =", fit_gev2$AIC, "\n")

# Return levels
cat("\nReturn levels (1929–2022, Gumbel):\n")
print(return.level(fit_gumbel2, return.period = c(10, 50, 100, 500)))

# Harvey exceedance (updated model)
params2 <- fit_gumbel2$results$par
mu2 <- params2["location"]; beta2 <- params2["scale"]

F2017_2 <- exp(-exp(-(Q2017 - mu2)/beta2))
p_exc2 <- 1 - F2017_2; T2017_2 <- 1/p_exc2

cat("\nHarvey 2017 peak =", Q2017, "cfs\n")
cat("Exceedance probability (1929–2022) =", p_exc2, "\n")
cat("Return period (years, 1929–2022) ≈", T2017_2, "\n")

# ============================
# Save Plots
# ============================
ggsave("hist_ecdf_combined_1929_2016.png",
       arrangeGrob(p_hist, p_ecdf, ncol=2),
       width=12, height=5)
ggsave("hist_ecdf_combined_1929_2022.png",
       arrangeGrob(p_hist2, p_ecdf2, ncol=2),
       width=12, height=5)
