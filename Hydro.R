# ---- 1) Gringorten function ----
gringorten <- function(Q) {
  Qs <- sort(as.numeric(Q), na.last = NA)  # remove NA if present
  n  <- length(Qs)
  i  <- seq_len(n)
  cdf <- (i - 0.44) / (n + 0.12)
  cbind(Q = Qs, cdf = cdf)
}

# ---- 2) (Optional) set working folder ----
setwd("C:/Users/z-ahm/OneDrive/Desktop/Hydro")  # use forward slashes

# ---- 3) Read data ----
a <- read.csv("gringorten_output.csv")   # must include .csv extension
# 3) Pick the peak-flow column (auto-detect common names; override if needed)
candidates <- c("PeakFlow","peakFlow","peak_va","PEAK_VA","peak","Q","discharge","flow")
peak_col <- intersect(names(a), candidates)
if (length(peak_col) == 0) stop(
  paste0("Could not find a peak-flow column. Columns seen: ", paste(names(a), collapse=", "))
)
Q <- a[[ peak_col[1] ]]
# 4) Compute CDF
cdfQ <- gringorten(Q)
cdfQ <- gringorten(Q)
cdfQ <- as.data.frame(cdfQ)
names(cdfQ) <- c("Q","cdf")  # ensure proper names

plot(cdfQ[["Q"]], cdfQ[["cdf"]],
     xlab = "Peak Flow (cfs)", ylab = "Empirical CDF",
     main = "Empirical CDF (Gringorten)", pch = 1)
lines(cdfQ[["Q"]], cdfQ[["cdf"]], col = "brown", lwd = 2)
