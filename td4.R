### Session 5 — Correction privilégiant data.table
library(data.table)
library(AER)      # pour EquationCitations
library(ggplot2)

# 0. Charger les données et passer en data.table --------------------
data("EquationCitations", package = "AER")
dt <- as.data.table(EquationCitations)   # nommage plus court

# Aperçu
dt[, .(pages, cites, journal)][1:10]

# 1) Régression linéaire simple: cites ~ pages ----------------------
regression_cit_pages <- lm(cites ~ pages, data = dt)
summary(regression_cit_pages)   # coefficients, R2, etc.

# 2) Variable: pour chaque article, moyenne des citations pour même pages
#    (moyenne conditionnelle sur pages)
dt[, mean_cites_by_pages := mean(cites, na.rm = TRUE), by = pages]

# 3) Graphique:
#    - points = citations par article
#    - ligne (ou points moyens) = moyenne des citations par nombre de pages
p1 <- ggplot(dt, aes(x = pages)) +
  geom_jitter(aes(y = cites), width = 0.2, height = 0.0, alpha = 0.6) +
  geom_point(aes(y = mean_cites_by_pages), color = "red", size = 2) +
  labs(y = "Citations (5 years)", title = "Citations vs pages (with mean by pages)")
print(p1)

# 4) Afficher les coefficients de la régression
coef(regression_cit_pages)        # intercept et slope

# 5) Moyenne des résidus et moyenne de pages
resids <- residuals(regression_cit_pages)
mean_resids <- mean(resids)
mean_resids

# 6) Corrélation entre résidus et pages
cor_resid_pages <- cor(resids, dt$pages, use = "complete.obs")
cor_resid_pages

# 7) Reprendre le graphique et y ajouter valeurs prédites
dt[, fitted_cit_pages := fitted(regression_cit_pages)]
p2 <- ggplot(dt, aes(x = pages)) +
  geom_jitter(aes(y = cites), width = 0.2, alpha = 0.4) +
  geom_line(aes(y = fitted_cit_pages), color = "blue", size = 1) +
  geom_point(aes(y = mean_cites_by_pages), color = "red", size = 2) +
  labs(y = "Citations", title = "Data, fitted regression and mean-by-pages")
print(p2)

# 8) Ré-estimer la régression en remplaçant cites par mean_cites_by_pages
#    (i.e. y_i := E[cites | pages = x_i])
regression_mean_by_pages <- lm(mean_cites_by_pages ~ pages, data = dt)
summary(regression_mean_by_pages)

# Interprétation : la régression sur la moyenne conditionnelle donne très
# la même pente (ou proche) que la régression sur les y individuels,

# 9) Produit cartésien : toutes paires d'articles --------------------
# Attention : taille = n^2. EquationCitations est modéré; toutefois, warning si grand.
n <- nrow(dt)
message("n = ", n)   # pour info
# construire deux tables i et j, puis cross join
dt_i <- dt[, .(i = .I, cites_i = cites, pages_i = pages)][,ident := "1"]
dt_j <- dt[, .(j = .I, cites_j = cites, pages_j = pages)][,ident := "1"]
cart <- merge(dt_i, dt_j, by = "ident", allow.cartesian = TRUE)

# 10) Calculer p_{ij} = (cites_i - cites_j) / (pages_i - pages_j)
#     Attention aux cas où pages_i == pages_j (division par 0) -> NA
cart[, p_ij := (cites_i - cites_j) / (pages_i - pages_j)]
# Exemples
cart[1:10, .(i,j,p_ij)][1:10]

# Interprétation : p_{ij} est la pente entre l'article i et j,
# c'est le taux de variation moyen des citations par page quand on passe de pages_j à pages_i.

# 11) Moyenne pondérée de p_{ij} avec poids ~ (pages_i - pages_j)^2
cart[, w := (pages_i - pages_j)^2]
# enlever les paires avec w == 0 (même nombre de pages) pour éviter NA dans p_ij
cart_nonzero <- cart[w != 0 & !is.na(p_ij)]
weighted_mean_pij <- sum(cart_nonzero$p_ij * cart_nonzero$w) / sum(cart_nonzero$w)
weighted_mean_pij

# Comparaison avec la pente OLS de lm(cites ~ pages)
coef(regression_cit_pages)["pages"]

# Explication théorique : la quantité calculée ci-dessus est numériquement égale
# au coefficient OLS obtenu par la régression linéaire simple. En effet :
# β_OLS = Σ_i Σ_j (y_i - y_j)(x_i - x_j) / Σ_i Σ_j (x_i - x_j)^2,
# d'où la moyenne pondérée ci-dessus.

# 12) Variance des résidus, variance des valeurs prédites, comparaison à var(cites)
var_resids <- var(resids, na.rm = TRUE)
var_fitted <- var(fitted(regression_cit_pages), na.rm = TRUE)
var_cites  <- var(dt$cites, na.rm = TRUE)
var_resids; var_fitted; var_cites ; var_fitted + var_resids

# Construction du R^2 : R^2 = 1 - Var(residuals) / Var(y)  (équivalent au R^2 classique pour OLS)
r2_from_vars <- 1 - var_resids / var_cites
r2_from_vars
summary(regression_cit_pages)$r.squared   # comparaison

# 13) Longueur moyenne des articles par journal
avg_pages_by_journal <- dt[, .(mean_pages = mean(pages, na.rm = TRUE), n = .N), by = journal]
avg_pages_by_journal[order(-mean_pages)]

# 14) Régression pages ~ journal (effet fixe de journal)
#     On peut utiliser factor(journal) dans lm ; data.table reste pour préparation.
regression_pages_journal <- lm(pages ~ factor(journal), data = dt)
summary(regression_pages_journal)
# Interprétation : coefficients = différences moyennes de pages par rapport à la revue de base (base = première modalité)

# Stocker l'objet
regression_pages_journal <- regression_pages_journal

# 15) Régression cites ~ journal
regression_cit_journal <- lm(cites ~ factor(journal), data = dt)
summary(regression_cit_journal)

# 16) Régression des résidus: résidus(reg_cit_journal) ~ résidus(reg_pages_journal)
resid_cit_journal  <- residuals(regression_cit_journal)
resid_pages_journal <- residuals(regression_pages_journal)
reg_resid_on_resid <- lm(resid_cit_journal ~ resid_pages_journal - 1) # sans intercept (optionnel)
summary(reg_resid_on_resid)

# 17) Comparer les coefficients : régression cite ~ pages + journal
reg_full <- lm(cites ~ pages + factor(journal), data = dt)
summary(reg_full)

# Résultat à constater : coefficient sur pages dans reg_full == coefficient dans la régression
# des résidus (résidus de cites~journal sur résidus de pages~journal) — c'est le théorème de Frisch-Waugh-Lovell.
coef(reg_full)["pages"]
# si on a mis -1 dans lm(resid...), la pente sera la même à machine près
coef(reg_resid_on_resid)

# 18) Pour chaque journal, estimer la régression cites ~ pages séparément,
#     puis calculer le nombre d'articles et la variance des pages, etc.
per_journal_stats <- dt[, .(
  n_articles = .N,
  var_pages = var(pages, na.rm = TRUE),
  slope_if_possible = if (.N > 1) coef(lm(cites ~ pages))[2] else NA_real_
), by = journal]

# Comment relier au coefficient global (question 17) ?
# Le coefficient sur "pages" dans reg_full est une moyenne pondérée
# (pondérée par la variance des pages *et* par la taille) des coefficients "within"
# (ce qui ressort de l'expression normale pour l'estimateur OLS avec dummies).
# Plus précisément, la composante "within-journal" (après avoir enlevé effets moyens par journal)
# est ce qui est mesuré par la régression des résidus (Frisch-Waugh-Lovell). Si les variances de pages
# par journal sont très différentes, alors les journaux avec grande variance de pages auront plus de poids
# dans l'estimation du coefficient pooled contrôlant pour journal.
per_journal_stats[,sum(slope_if_possible * n_articles * var_pages * (n_articles - 1) / n_articles) / sum(n_articles * var_pages * (n_articles - 1) / n_articles)]
coef(reg_full)[2]

# Récapitulatif rapide d'interprétations :
# - p_ij = pente "séquente" entre deux articles i et j.
# - la moyenne pondérée des p_ij avec poids (x_i-x_j)^2 = pente OLS de cites~pages
# - quand on ajoute journal dans la régression, c'est comme si on faisait la moyenne de régressions séparément pour chaque revue
# - donc la regression cites~pages+journal correspond à une moyenne faite de comparaisons entre articles de la même revue