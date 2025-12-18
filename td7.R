# TD : tests usuels + Monte-Carlo
# Favorise data.table et vectorisation
set.seed(12345)

# Packages ----
library(data.table)
library(AER)       # pour CPS1985
library(lmtest)    # coeftest
library(sandwich)  # vcovHC, etc.
library(car)       # linearHypothesis

# ------------------------------------------------------------------
# PROBLEME 1 : CPS1985 - tests usuels
# ------------------------------------------------------------------

# 1) Charger les données CPS1985
data("CPS1985", package = "AER")
dt <- as.data.table(CPS1985)

# Aperçu
dt[,female := as.numeric(gender == "female")]
dt[, .(wage, female, education)][1:6]

# 2) Estimer wage ~ female + education
reg1 <- lm(wage ~ female + education, data = dt)

# 3) Intervalles de confiance 95% (utilise vcovHC si on veut robuste)
# par défaut, coeftest / confint utilise la vcov classique ; on calcule explicitement
coef_est <- reg1$coefficients
vcov_classic <- vcov(reg1)
se_classic <- sqrt(diag(vcov_classic))
ci_95_lower <- coef_est - qnorm(0.975) * se_classic
ci_95_upper <- coef_est + qnorm(0.975) * se_classic
ci_95 <- cbind(ci_95_lower, ci_95_upper)
colnames(ci_95) <- c("2.5%", "97.5%")
ci_95

#Comparaison aux intervalles construits avec coefci
coefci(reg1)

# Si on veut intervalles robustes
vcov_HC0 <- vcovHC(reg1)
se_HC0 <- sqrt(diag(vcov_HC0))
ci_95_HC0 <- cbind(coef_est - qnorm(0.975) * se_HC0,
                   coef_est + qnorm(0.975) * se_HC0)
ci_95_HC0

#Comparaison aux intervalles de confiance construits avec coefci
coefci(reg1,
       vcov. = vcovHC)

#La différence vient de l'approximation normale que l'on fait dans la
# construction à la main
#coefci utilise la loi de Student cf slides du cours

# 4) Intervalles de confiance 99%
ci_99 <- coefci(reg1,
                vcov. = vcovHC,
                level = 0.99)
ci_99

# 5) Créer variables d'éducation spécifiques par sexe
# education_male = education for males, 0 for females
# education_female = education for females, 0 for males
dt[, education_male := ifelse(female == 0, education, 0)]
dt[, education_female := ifelse(female == 1, education, 0)]

# 6) Régression wage ~ female + education_male + education_female
reg2 <- lm(wage ~ female + education_male + education_female, data = dt)

# 7) Tester nullité simultanée des coefficients sauf intercept (F-test)
# H0: coefficients on female, education_male, education_female = 0
lh1 <- linearHypothesis(reg2,
                        c("female = 0",
                          "education_male = 0",
                          "education_female = 0"),
                        singular.ok = TRUE,
                        vcov. = vcovHC)
lh1

# 8) Tester égalité des coefficients des deux variables d'éducation
# H0: education_male = education_female
lh2 <- linearHypothesis(reg2, "education_male = education_female", vcov. = vcovHC)
lh2

# 9) Peut-on tester la même hypothèse par un test sur un seul coefficient ?
# Oui : réparamétrer la régression (par ex. inclure education et interaction female*education)
# Variante : estimer wage ~ female + education + female:education et tester interaction = 0
reg3 <- lm(wage ~ female * education, data = dt)
reg3$coefficients
# test H0: coefficient of female:education = 0
coeftest(reg3, vcov. = vcovHC)

# ------------------------------------------------------------------
# PROBLEME 2 : Monte-Carlo
# ------------------------------------------------------------------

# Paramètres de la simulation
nsim  <- 1000     # nombre de générations
n     <- 500      # taille de chaque génération
k     <- 2        # nombre de régressors (constante + x)

liste_simulations <-
  lapply(X = 1:nsim,
         FUN = function(blank) 
           data.table::data.table(
           x = runif(n = n,
                     min = 0,
                     max = 1),
           epsilon0 =
             rnorm(n = n,
                   mean = 0,
                   sd = 1)
           )
         )

liste_simulations <-
  lapply(X = liste_simulations,
         FUN = function(table)
           table[,
                 epsilon := epsilon0 * x])

liste_simulations <-
  lapply(X = liste_simulations,
         FUN = function(table)
           table[,
                 c("y1",
                   "y2") :=
                   list(3 + 2 * x + epsilon,
                        4 + epsilon)])

liste_simulations[[1]]

liste_reg1 <-
  lapply(X = liste_simulations,
         FUN = function(table)
           lm(y1 ~ x,
              data = table))

hatbeta1 <-
  unlist(
    lapply(X = liste_reg1,
           FUN = function(regression)
             regression$coefficients["x"]))

hatsigma1homo <-
  unlist(
    lapply(
      X = liste_reg1,
      FUN = function(regression)
        sqrt(vcov(regression)["x","x"])))

hatsigma1hetero <-
  unlist(
    lapply(
      X = liste_reg1,
      FUN = function(regression)
        sqrt(vcovHC(regression)["x","x"])))

ci1homo <-
    data.table::rbindlist(
      lapply(
        X = 1:nsim,
        FUN = function(index)
          data.table::data.table(
            LB = coefci(liste_reg1[[index]])["x",1],
            UB = coefci(liste_reg1[[index]])["x",2])))

ci1hetero <-
  data.table::rbindlist(
    lapply(
      X = 1:nsim,
      FUN = function(index)
        data.table::data.table(
          LB = coefci(liste_reg1[[index]],
                      vcov. = vcovHC)["x",1],
          UB = coefci(liste_reg1[[index]],
                      vcov. = vcovHC)["x",2])))

mean(hatbeta1)
sd(hatbeta1)

ci1homo[,contains2 :=
          as.numeric(2 >= LB
                     & 2 <= UB)]
mean(ci1homo$contains2)

ci1hetero[,contains2 :=
          as.numeric(2 >= LB
                     & 2 <= UB)]
mean(ci1hetero$contains2)

liste_reg2 <-
  lapply(X = liste_simulations,
         FUN = function(table)
           lm(y2 ~ x,
              data = table))

hatbeta2 <-
  unlist(
    lapply(X = liste_reg2,
           FUN = function(regression)
             regression$coefficients["x"]))

hatsigma2homo <-
  unlist(
    lapply(
      X = liste_reg2,
      FUN = function(regression)
        sqrt(vcov(regression)["x","x"])))

hatsigma2hetero <-
  unlist(
    lapply(
      X = liste_reg2,
      FUN = function(regression)
        sqrt(vcovHC(regression)["x","x"])))

student2homo <-
  unlist(
    lapply(
      X = liste_reg2,
      FUN = function(regression)
        as.numeric(abs(coeftest(regression)["x","t value"]) >
                     LaplacesDemon::qst(0.975, nu = n - 2))
      ))
mean(student2homo)

student2hetero <-
  unlist(
    lapply(
      X = liste_reg2,
      FUN = function(regression)
        as.numeric(abs(coeftest(regression,
                                vcov. = vcovHC)["x","t value"]) >
                     LaplacesDemon::qst(0.975, nu = n - 2))
    ))
mean(student2hetero)
