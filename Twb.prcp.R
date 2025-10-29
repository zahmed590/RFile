# ============================================================
# 1️⃣ Wet-Bulb Temperature Function
# ============================================================
TWb <- function(X, z) {
  Ta <- X[1]  # Air temperature in °C
  RH <- X[2]  # Relative humidity (0–1)
  
  # Atmospheric pressure at elevation z (kPa)
  pz <- 101.3 * exp(-0.00013 * z)
  
  # Saturation vapor pressure (kPa)
  expt <- exp(17.3 * Ta / (Ta + 237.3))
  estar <- 0.611 * expt
  
  # Slope of saturation vapor pressure curve (kPa/°C)
  Delt <- (2508.3 / (Ta + 237.3)^2) * expt
  
  # Wet-bulb temperature (°C)
  twb <- Ta - (estar * (1 - RH)) / Delt + 0.000643 * pz
  
  return(twb)
}

# ============================================================
# 2️⃣ Rain–Snow Classification Function
# ============================================================
Rainsnow <- function(X, Z) {
  Ta <- X[1]
  RH <- X[2]
  
  # Threshold temperature (To)
  To <- -5.87 - 1.042e-04 * Z + 8.85e-08 * Z^2 + RH * 16.06 - 9.614 * RH^2
  
  if (RH > 0.78) {
    Tmin <- To - 11.756 - 23.1 * RH + 10.289 * RH^2
    Tmax <- 2 * To - Tmin
  } else {
    Tmin <- To
    Tmax <- To
  }
  
  # Compute wet-bulb temperature
  Twb <- TWb(X, Z)
  
  # Classification
  if (Twb <= Tmin) {
    idx <- "Snow"
  } else if (Twb > Tmin & Twb < Tmax) {
    idx <- "Rain"
  } else {
    idx <- "Sleet"
  }
  
  return(idx)
}

# ============================================================
# 3️⃣ Load Data and Prepare Inputs
# ============================================================
setwd("D:/Elma/Amerilo")
a <- read.csv("Amarillo.csv", header = TRUE)

# Example column names (adjust to your actual file)
# Suppose 'TF' = Temperature in °F and 'RHPER' = Relative humidity (%)
TF <- a$TF
RH <- a$RHPER / 100
TC <- (TF - 32) * (5 / 9)  # Convert °F → °C

# Combine columns into matrix for apply()
X <- cbind(TC, RH)

# Convert elevation (ft → m)
zft <- 3662
z <- zft * 0.3048

# ============================================================
# 4️⃣ Apply Function Row-wise
# ============================================================
RSS <- apply(X, 1, Rainsnow, Z = z)

# Add to dataframe and save
a$PrecipType <- RSS
write.csv(a, "Amarillo_RainSnow.csv", row.names = FALSE)
print(head(a))
