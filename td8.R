# Session 9 — Solutions (data.table + lfe)
# Required packages
library(data.table)
library(plm)     # for the Males dataset
library(lfe)     # felm
library(lmtest)  # coeftest
library(sandwich) # robust vcov

# 0. Load data --------------------------------------------------------
data("Males", package = "plm")
DT <- as.data.table(Males)   # keep name short

# Quick check of column names and sizes
print(dim(DT))
print(names(DT))

# 1. For each individual, compute three quantities:
#    a) the difference between their mean log-wage when union==1 and when union==0
#    b) the share of time spent in union jobs (mean of union)
#    c) the individual variance of the union indicator

by_id <- DT[, .(
  mean_wage_union    = mean(wage[union == "yes"], na.rm = TRUE),
  mean_wage_nonunion = mean(wage[union == "no"], na.rm = TRUE),
  share_union        = mean(as.numeric(union == "yes"), na.rm = TRUE),
  var_union          = var(as.numeric(union == "yes"), na.rm = TRUE),
  n_obs              = .N
), by = nr]

# compute the difference mean_union - mean_nonunion
by_id[, mean_diff := mean_wage_union - mean_wage_nonunion]

# Show a few lines
by_id[1:10]

# 2. Proportion of missing values for the individual mean difference
#    (missing when an individual has no union==1 or no union==0 observations)
n_total_ids <- nrow(by_id)
n_missing_diff <- by_id[is.na(mean_diff), .N]
prop_missing <- n_missing_diff / n_total_ids
cat("Number of individuals:", n_total_ids, "\n")
cat("Number with missing mean_diff:", n_missing_diff, 
    " (proportion = ", round(prop_missing, 3), ")\n", sep="")

# Explanation: missing occurs for individuals who are always union==0 or always union==1,
# so one of mean_wage_union / mean_wage_nonunion is undefined.

# 3. Weighted mean of the individual differences using weights proportional to var(union)
#    (note: exclude NA mean_diff and exclude zero variance to avoid zero weights)
by_id_non_na <- by_id[!is.na(mean_diff) & !is.na(var_union) & var_union > 0]
weighted_mean_diff <- weighted.mean(by_id_non_na$mean_diff, w = by_id_non_na$var_union)
weighted_mean_diff

# Interpretation note: weighting by var(union) gives more weight to individuals
# whose union status varies across time (i.e. informative within-individual variation).
# This weighted mean targets a population where within-individual variation is higher.

# 4. OLS regression of log-wage on union and individual dummies (indicator for each worker)
#    Equivalent to: wage_it = alpha + beta * union_it + sum_j gamma_j * 1{nr=j} + u_it
#    We can estimate with lm using factor(nr), but this is heavy
reg_union_with_dummies <- lm(wage ~ union + factor(nr), data = DT)  # union + individual fixed effects
reg_union_with_dummies$coefficients["unionyes"]

# Interpretation of coefficient on 'union':
#  - This is the within-individual effect: the average change in log-wage when an individual
#    switches from non-union to union employment, controlling for individual fixed effects.

# Compare coefficient to the sample average of individual differences computed earlier:
all.equal(
  weighted_mean_diff,
  as.numeric(reg_union_with_dummies$coefficients["unionyes"])
)

# These two figures should be comparable: the within estimator equals a weighted
# average of pairwise difference with the same weighting

# 5. Regression of union_it on share_union_i (a time-constant per individual)
#    At observation level: union_it ~ share_i
#    The slope should be 1 (intuitively), let's verify:
#    Construct share per individual in DT
DT[, share_union := mean(as.numeric(union == "yes"), na.rm = TRUE), by = nr]

# Regress union_it on share_union
reg_union_on_share <- lm(as.numeric(union == "yes") ~ share_union, data = DT) # theory predicts slope ~ 1
reg_union_on_share$coefficients
# If you include an intercept: lm(union ~ share_union, data=DT) you'd get intercept ~ 0 and slope ~1

# Explanation: union_it = share_i + (union_it - share_i). Covariance structure implies slope = 1.
# Other explanation: share_i is the best approximation of union_it that only depends on individual-level dummies
# The predicted values of the regression are the best approximation of union_it that only depends on share_i
# If the coefficients differed from 1 then we would get beta * share_i that would be a better approximation of union_it than share_i...

# 6. Regression of wage on union and share_union together:
reg_wage_union_share <- lm(wage ~ union + share_union, data = DT)
reg_wage_union_share$coefficients

# Interpretation:
# - 'union' coefficient captures within-individual effect (deviation from individual's mean share)
# - 'share_union' captures cross-sectional (between) effect
all.equal(
  as.numeric(reg_union_with_dummies$coefficients["unionyes"]),
  as.numeric(reg_wage_union_share$coefficients["unionyes"])
)

# 7. felm with individual fixed effects: wage ~ union | nr
reg_felm_union_fe <- felm(wage ~ union | nr, data = DT)
reg_felm_union_fe$coefficients

# Compare coefficient with previous:
all.equal(
  as.numeric(reg_felm_union_fe$coefficients["unionyes"]),
  as.numeric(reg_union_with_dummies$coefficients["unionyes"])
)

# 8. Which variance-covariance matrix to use?
#    For within FE estimates, standard errors should account for clustering if observations
#     because the sampling obviously took place at the individual level and not the indiviudal*year level
reg_felm_union_fe <- felm(wage ~ union | nr | 0 | nr, data = DT)
coefci(reg_felm_union_fe)

# 9. Regression of wage on school with individual fixed effects:
#    If 'school' is time invariant per individual, then it is collinear with the individual FE
#    and will be dropped by the within estimator. Let's check:
# Check variation of school by individual
school_by_id <- DT[, .(var_school = var(school, na.rm = TRUE), n = .N), by = nr]
summary(school_by_id$var_school)
# If only zeros, school is time-invariant -> felm will drop it.

# Run felm and see what happens
reg_school_fe <- felm(wage ~ school | nr | 0 | nr, data = DT)
summary(reg_school_fe)
# Expect that 'school' is dropped (no within variation), or that the coefficient is NA.

# 10. Regression of wage on exper and year with individual fixed effects:
#     Potential issue: 'year' is a time effect (common to all), 'exper' may be collinear with 'year'
#     once we condition on individual FE
reg_exper_year_fe <- felm(wage ~ exper + year | nr | 0 | nr, data = DT)
summary(reg_exper_year_fe)
# Check for warnings about multicollinearity or dropped variables.

# 11. felm with union and fixed effects defined at individual (nr) × industry
#     We can include both nr and industry fixed effects: | nr + industry
reg_union_nr_ind <- felm(wage ~ union | factor(nr):industry | 0 | nr, data = DT)
reg_union_nr_ind$coefficients

# Interpretation of coef on 'union':
# - It measures the within-nr-and-within-industry effect of union on wage,
#   i.e. comparing observations of the same worker and same industry over time

# 12. Which VCOV to use for the previous estimation?
#     The sampling is still done at the individual level so that is the relevant level!
coefci(reg_union_nr_ind)
