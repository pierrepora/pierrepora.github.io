# ============================================================
# Corrigé du TD : Hétéroscédasticité et matrice de variance-covariance
# ============================================================

# Chargement des packages nécessaires
library(AER)
library(data.table)

# ============================================================
# 1. Chargement des données
# ============================================================

data("EquationCitations")
dt <- as.data.table(EquationCitations)

# Aperçu
head(dt)

# ============================================================
# 2. Graphique : citations (ordonnée) vs pages (abscisse)
# ============================================================

plot(dt$pages, dt$cites,
     main = "Nombre de citations en fonction du nombre de pages",
     xlab = "Nombre de pages",
     ylab = "Nombre de citations")

# ============================================================
# 3. Variance du nombre de citations pour chaque longueur d'article
# ============================================================

var_cites <- dt[, .(var_cites = var(cites)), by = pages]

# ============================================================
# 4. Représentation graphique de la variance conditionnelle
# ============================================================

plot(var_cites$pages, var_cites$var_cites, 
     main = "Variance conditionnelle du nombre de citations",
     xlab = "Nombre de pages", ylab = "Variance des citations")

# ============================================================
# 5. Régression cites ~ pages
# ============================================================

reg_cit_pages <- lm(cites ~ pages, data = dt)
reg_cit_pages

# ============================================================
# 6. Ajout des résidus à la table
# ============================================================

dt[, resid := residuals(reg_cit_pages)]

# ============================================================
# 7. Moyenne des résidus et moyenne des résidus au carré par longueur
# ============================================================

resid_stats <- dt[, .(
  mean_resid_sq = mean(resid)^2,
  mean_sq_resid = mean(resid^2),
  var_cites = (.N - 1) / .N * var(cites)
), by = pages]

head(resid_stats)

resid_stats$mean_sq_resid - resid_stats$mean_resid_sq - resid_stats$var_cites

# ============================================================
# 8. Graphique de la moyenne du carré des résidus
# ============================================================

plot(resid_stats$pages, resid_stats$mean_sq_resid, 
     main = "Moyenne du carré des résidus selon la longueur des articles",
     xlab = "Nombre de pages", ylab = "Moyenne du carré des résidus")

# ============================================================
# 9. Nouvelle régression avec option x = TRUE
# ============================================================

reg_cit_pages <- lm(cites ~ pages, data = dt, x = TRUE)
# L’option x = TRUE permet de récupérer la matrice des régresseurs :
#   reg_cit_pages$x

# ============================================================
# 10. Estimation empirique de E[XX']
# ============================================================

X <- as.matrix(reg_cit_pages$x)
EXX <- crossprod(X) / nrow(X)
EXX

# ============================================================
# 11. Matrice diagonale des résidus au carré
# ============================================================

diag_resid_sq <- diag(dt$resid^2)

# ============================================================
# 12. Estimation empirique de E[XX'ε²]
# ============================================================

EXXeps2 <- t(X) %*% diag_resid_sq %*% X / nrow(X)
EXXeps2

# ============================================================
# 13. Estimation de la matrice de variance-covariance asymptotique
# ============================================================

# Var(β_hat) = (E[XX'])^{-1} * E[XX'ε²] * (E[XX'])^{-1}
V_hat <- solve(EXX) %*% EXXeps2 %*% solve(EXX)
V_hat

# ============================================================
# 14. Comparaison avec vcovHC(type = "HC0")
# ============================================================

V_HC0 <- vcovHC(reg_cit_pages, type = "HC0")
V_HC0

all.equal(
  1 / nrow(X) * V_hat,
  V_HC0
)

# ============================================================
# 15. Comparaison avec la matrice classique (vcov)
# ============================================================

V_classique <- vcov(reg_cit_pages)
V_classique

# ============================================================
# 16. Écarts-types donnés par coeftest (par défaut)
# ============================================================

coeftest(reg_cit_pages)  # par défaut : variance non robuste

# ============================================================
# 17. Écarts-types robustes avec la matrice de la Q11
# ============================================================

# Création d’un objet coeftest avec notre matrice robuste manuelle :
coeftest(reg_cit_pages, vcov. = 1 / nrow(X) * V_hat)

# ============================================================
# 18. Différence entre HC0 et HC1
# ============================================================

V_HC1 <- vcovHC(reg_cit_pages, type = "HC1")

# La matrice HC1 = HC0 * (n / (n - k))
# Vérification :
n <- nrow(X)
k <- ncol(X)
all.equal(V_HC1, V_HC0 * (n / (n - k)))
