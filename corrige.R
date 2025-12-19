#1.1

library(AER)

data("CPS1985")

CPS1985 <- data.table::data.table(CPS1985)

#1.2

CPS1985_restreint <- CPS1985[age >=  25 & age <= 55]

#1.3

CPS1985_restreint[,female := as.numeric(gender == "female")]

#1.4

salaire_par_sexe <- 
  CPS1985_restreint[,
          list(salaire_moyen = mean(wage),
               salaire_ecart_type = sqrt(var(wage)),
               nobs = .N),
          by = c("gender")]

salaire_par_sexe

#1.5

salaire_par_education <-
  CPS1985[,
          list(salaire_moyen = mean(wage)),
          by = c("education")]

#1.6

ggplot2::ggplot(data = salaire_par_education,
                ggplot2::aes(x = education,
                    y = salaire_moyen)) +
  ggplot2::geom_point()

#2.1

reg1 <-
  lm(wage ~ female + education,
     data = CPS1985_restreint)

#2.2

#beta2 est la moyenne (avec des poids proportionnels à la part de chaque sexe dans la population), et à la variance
# de l'éducation dans chaque groupe de sexe, des coefficients
# de la régression du salaire horaire sur l'éducation effectuée séparément pour chaque sexe. Ces coefficients 
# spécifiques à chaque sexe sont des moyennnes de comparaisons (pente de la droite qui passe par les deux points)
# deux-à-deux d'individus de même sexe mais ayant des niveaux d'éducation différents avec des poids proportionnels
# au carré de leur écart d'éducation

#2.3

coefci(reg1,
       vcov. = vcovHC)

#2.4

#La matrice de covariance à utiliser ici est la matrice de variance-covariance générale, sans hypothèse 
# d'homoscédasticité. La fonction lm fait ici l'hypothèse d'homoscédasticité, qui n'est pas crédible de façon
# générale, et en particulier dans ce contexte où les salaires sont plus dispersés pour les individus ayant
# un niveau d'éducation plus élevé

plot(CPS1985_restreint[,list(var_salaire = var(wage)), by = c("education","gender")][,c("education","var_salaire")])

#2.5

coeftest(reg1,
         vcov. = vcovHC)["female",]

#On rejette l'hypothèse au seuil de 5%

#2.6

reg2 <-
  lm(wage ~ female + education + education * female,
     data = CPS1985_restreint)

#2.7

#Le coefficient sur la variable d'éducation correspond au coefficient du salaire sur l'éducation dans une régression 
# simple restreinte aux hommes

reg_hommes <-
  lm(wage ~education,
     data = CPS1985_restreint[female == 0])

all.equal(reg2$coefficients["education"],
          reg_hommes$coefficients["education"])

#2.8

#Le coefficient sur education dans reg2 est le coefficient pour la régression simple spécifique aux hommes
#Le coefficient pour la régression simple spécifique aux femmes est la somme de ce coefficient et du coefficient sur
# l'interaction
#L'ancien coefficient beta_2 est donc égal à une moyenne du nouveau coefficient sur l'éducation, et d'une somme
# de ce coefficient et du coefficient sur l'interaction, avec des poids proportionnels à la part de chaque sexe dans
# l'emploi salairé, et à la variance de l'éducation pour ce sexe
#Il est donc égal au nouveau coefficient sur l'éducation, plus un terme qui fait intervenir le poids relatif des femmes
# (au sens de la part dans la population et de la variance de l'éducation) dans cette régression, multipliée par 
# le terme d'interaction

part_femme <- mean(CPS1985_restreint$female)
nb_femme <- nrow(CPS1985_restreint[female == 1])
nb_homme <- nrow(CPS1985_restreint[female == 0])
var_educ_female <- (nb_femme - 1) / nb_femme * var(CPS1985_restreint[female == 1]$education)
var_educ_male <- (nb_homme -1 ) / nb_homme *var(CPS1985_restreint[female == 0]$education)

coefficient_reconstruit <-
  reg2$coefficients["education"] + 
  part_femme * var_educ_female / (part_femme * var_educ_female + (1 - part_femme) * var_educ_male) * reg2$coefficients["female:education"]

all.equal(
  as.numeric(reg1$coefficients["education"]),
  as.numeric(coefficient_reconstruit))

#3.1

library(plm)

data("LaborSupply")

head(LaborSupply)
help(LaborSupply)

#La variable id est un identifiant qui permet de s'assurer qu'on suit le même individu au cours de différentes annés
# C'est une variable essentielle pour pouvoir faire des comparaisons longitudinales

#3.2

reg_panel_1 <-
  lfe::felm(lnhr ~ age + year | id | 0 | id,
          data = LaborSupply)

reg_panel_1$coefficients
#Les coefficients de cette régression ne devraient pas pouvoir être estimés conjointement : chaque année
# le temps calendaire augment d'une an et l'âge des individus suivis augmente d'un an aussi donc les comparaisons
# longitudinales sont colinéaires
# Il doit y avoir des erreurs dans le report de l'âge

LaborSupply <- data.table::data.table(LaborSupply)
LaborSupply[,age_1979 := sum(age * as.numeric(year == 1979)), by = c("id")]

pb_age <- 
  LaborSupply[age != age_1979 + year - 1979]
summary(pb_age[,age - age_1979 - year + 1979])
#Ces erreurs sont d'une seule année, cela pourrait arriver si l'âge est mesuré à la date exacte de l'enquête et que
# celle-ci n'est pas réalisée au même moment tous les ans (concept d'âge en différence de millésime vs. âge exact)
#Caricaturalement si elle est réalisée parfois en décembre et parfois en janvier
#Ceci dit certaines différences sont bien plus importantes ce qui suggère des erreurs de mesure

LaborSupply[,age_diff_mill := age_1979 + year - 1979]

reg_panel_1_bis <-
  lfe::felm(lnhr ~ age_diff_mill + year | id | 0 | id,
            data = LaborSupply)

reg_panel_1_bis$coefficients
#On retombe bien sur le problème de colinéarité quand on considère l'âge en différence de millésime

#3.3

reg_panel_2 <-
  lfe::felm(lnhr ~ disab | id | 0 | id,
            data = LaborSupply)

#3.4

#Le coefficient gamma est une moyenne de comparaisons longitudinales spécifiques à chaque individus entre les 
# périodes où il est en mauvaise santé (disab == 1) et celles où il est en bonne santé (disab == 0). Comme on est
# sur un panel cylindré,les poids spécifiques à chaque individus sont proportionnels à la variance individuelle de
# disab. Elle vaut 0 pour les individus qui ne changent jamais de statut (disab == 1 tout 
# au long de la période d'observation, ou disab == 0 tout au long de la période d'observation). Le coefficient ne porte
# donc que sur ceux qui changent de statut, les movers, et il met davantage de poids sur les personnes qui passent la 
# moitié de la période d'observation en bonne santé, et l'autre moitié en mauvaise santé
#L'intérêt du logarithme est que l'on a une interprétation presque directe en termes de variation relative plutôt
# qu'absolue des heures travaillées

statut <-
  LaborSupply[,
              list(mover =as.numeric(var(disab) != 0)),
              by = c("id")]
part_movers <- mean(statut$mover)
part_movers

#Les movers représentent 20% de la population suivie

#3.5

coefci(reg_panel_2)

#Il faut utiliser ici la matrice de variance-covariance qui prend en compte le caractère groupé des données
# L'unité échantillonnée est bien un individu, ensuite suivi au cours du temps, et pas une observation
# individu-année. Cette matrice de variance-covariance a été spécifiée dans l'appel de la fonction lfe
# donc on n'a pas besoin de le réécrire ici

#3.6

#Une stratégie alternative serait de procéder en premières différences, c'est-à-dire en comparant chaque année à 
# l'année précédente

data.table::setorder(LaborSupply,
                     id,
                     year)

LaborSupply[,
            paste0(c("disab", 
                     "lnhr"),
                   "_diff") :=
              lapply(X = c("disab","lnhr"),
                     FUN = function(var)
                       data.table::fcase(
                         data.table::shift(id) == id,
                         get(var) - data.table::shift(get(var)),
                         default = NA_real_))]

head(LaborSupply)

reg_premiere_diff <-
  lm(lnhr_diff ~ disab_diff - 1,
     data = LaborSupply)

#Le coefficient obtenu coïncide exactement avec celui donné par l'estimateur within dans le cas où
# on ne travaille que sur deux périodes

reg_panel_2_deux_periodes <-
  lfe::felm(lnhr ~ disab |id|0|id,
            data = LaborSupply[year %in% c(1979,1980)])
reg_premiere_diff_deux_periodes <-
  lm(lnhr_diff ~disab_diff - 1,
     data = LaborSupply[year == 1980])

all.equal(
  as.numeric(reg_panel_2_deux_periodes$coefficients[1]),
  as.numeric(reg_premiere_diff_deux_periodes$coefficients["disab_diff"]))

#Dans la régression proposée, on ne fait pas de comparaisons longitudinales, mais des comparaisons transversales
#Au lieu de comparer les mêmes individus, entre des moments où ils sont en bonne et mauvaise santé, on compare 
# les heures travaillées par des individus en bonne et mauvaise santé la même année, puis on fait la moyenne de ces
# comparaisons spécifiques à chaque année en mettant plus de poids sur les années où la part de personnes en mauvaise
# santé est plus proche de 50%

reg_coupe_repetee <-
  lfe::felm(lnhr ~disab | factor(year) | 0 | id,
            data = LaborSupply)

#3.8

coeftest(reg_coupe_repetee)[,"Pr(>|t|)"]

#Il faut continuer à utiliser la matrice de variance-covariance adaptée aux données groupées parce que c'est 
# toujours comme ça qu'a été produit l'échantillon ! Le tirage se fait au niveau des individus, pas au niveau des 
# individus-années.
