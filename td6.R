###############################################
# Correction – Session 7 : Linear regression with clustered data
###############################################

# Packages ----
library(data.table)
library(haven)
library(geosphere)
library(lmtest)
library(sandwich)

# 1. Load the data ----
endowment <- as.data.table(read_dta("./TA sessions/endowment_data.dta"))

# Quick overview
head(endowment)
str(endowment)

# 2. Compute the distance to Mangola ----
# Coordinates of Mangola village
mangola_coords <- cbind(-3.519, 35.33)  # order is longitude, latitude

# Compute great-circle distance for each camp
endowment[, dist_mangola := geosphere::distm(
  x = cbind(gpsx, gpsy),
  y = mangola_coords,
  fun = geosphere::distHaversine
)]

# Convert to kilometers for readability
endowment[, dist_mangola := dist_mangola / 1000]

# 3. Reshape the dataset to long format ----
# Assume variables: endowment_food, endowment_lighter
# (indicating whether the endowment effect was observed in each experiment)

endowment_long <- melt(
  endowment,
  id.vars = c("campname", "age", "sex", "dist_mangola"),
  measure.vars = c("endowmentbiscuit", "endowmentlighter"),
  variable.name = "experiment",
  value.name = "endowment_effect"
)

endowment_summary <-
  endowment_long[,
                 list(share = mean(as.numeric(endowment_effect == "Yes")),
                      size = .N,
                      dist_mangola = median(dist_mangola)),
                 by = c("campname")]

ggplot2::ggplot(data = endowment_summary,
                ggplot2::aes(x = dist_mangola,
                    y = share,
                    size = size)) +
  ggplot2::geom_point()

# 4. Create the exposure variable ----
# Find the 4 camps closest to Mangola
closest_camps <- endowment_long[
  , .(mean_dist = mean(dist_mangola)), by = campname
][order(mean_dist)][1:4, campname]

endowment_long[
  , exposure := ifelse(campname %in% closest_camps, "high", "low")
]

# Convert to factor (optional)
endowment_long[, exposure := factor(exposure, levels = c("low", "high"))]

# 5. Estimate the regression ----
reg_endowment <- lm(as.numeric(endowment_effect == "Yes") ~ exposure, data = endowment_long)
summary(reg_endowment)

# 6. Discussing inference ----
# By default, vcov() assumes homoskedasticity.
# For data clustered by camp, this may understate true variability.

# 7. Cluster-robust variance–covariance matrix ----
# Compute cluster-robust standard errors at the camp level
vcov_camp <- sandwich::vcovCL(reg_endowment, cluster = endowment_long$camp)

# Test with clustered standard errors
lmtest::coeftest(reg_endowment, vcov = vcov_camp)

# 8. Discussion ----
# Clustering at the camp level is natural because observations within a camp
# may share unobserved characteristics (social norms, exposure, etc.).
# However, the number of clusters is small (only a few camps), which limits
# the reliability of asymptotic inference.

###############################################
# End of correction
###############################################
