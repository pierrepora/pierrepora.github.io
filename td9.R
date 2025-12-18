library(AER)

#Load the FT-PT data (https://data-explorer.oecd.org/vis?lc=en&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_FTPT%40DF_FTPT_COMMON&df[ag]=OECD.ELS.SAE&df[vs]=1.0&dq=.EMP.PS._T..EMP.MAIN._T.FT%2BPT..A&pd=%2C&to[TIME_PERIOD]=false)
ft_pt <-
  data.table::fread("./OECD.ELS.SAE,DSD_FTPT@DF_FTPT_COMMON,1.0+all.csv")
#Load the gender wage gap data (https://data-explorer.oecd.org/vis?lc=en&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_EARNINGS%40GENDER_WAGE_GAP&df[ag]=OECD.ELS.SAE&dq=......_T&pd=2015%2C&to[TIME_PERIOD]=false)
gwg <-
  data.table::fread("./OECD.ELS.SAE,DSD_EARNINGS@GENDER_WAGE_GAP,+all.csv")

#Restrict the FT PT table to the total population, and to sex-specific observations
ft_pt <-
  ft_pt[AGE == "_T"
        & SEX != "_T"]

# Compute the share
share_pt_gender <-
  ft_pt[,
        list(share = sum(as.numeric(WORK_TIME_ARNGMNT == "PT") * OBS_VALUE / sum(OBS_VALUE))),
        by = c("REF_AREA",
               "TIME_PERIOD",
               "SEX")
          ]

#Compute the ratio
ratio_share_pt_gender <-
  share_pt_gender[,
                  list(ratio = exp(sum((2 * as.numeric(SEX == "F") - 1) * log(share)))),
                  by = c("REF_AREA",
                         "TIME_PERIOD")]
#Restrict de gwg table to gender gap in average wages
gwg_mean <-
  gwg[AGGREGATION_OPERATION == "MEAN"]

colonnes_a_conserver <-
  c("REF_AREA",
    "TIME_PERIOD",
    "OBS_VALUE")

#Merge the two datasets
gwg_pt <-
  merge(x = ratio_share_pt_gender,
        y = gwg_mean[,..colonnes_a_conserver],
        by = c("REF_AREA",
               "TIME_PERIOD"),
        all = FALSE)

colnames(gwg_pt)[colnames(gwg_pt) == "OBS_VALUE"] <-
  "gwg"

#Number of years
length(unique(gwg_pt$TIME_PERIOD))

#Number of countries
length(unique(gwg_pt$REF_AREA))

#Figure

ggplot2::ggplot(data = gwg_pt,
                ggplot2::aes(x = ratio,
                             y = gwg,
                             color = TIME_PERIOD)) + 
  ggplot2::geom_point() + 
  viridis::scale_color_viridis(option = "magma") + 
  ggplot2::theme_classic()

#Pooled OLS regression

reg_coupe_repetee <-
  lm(gwg ~ ratio + factor(TIME_PERIOD),
     data = gwg_pt[TIME_PERIOD >= 1980])

#Confidence intervals
coefci(reg_coupe_repetee,
         vcov. = vcovHC)

clustered_vcov <-
  vcovCL(reg_coupe_repetee,
         cluster = gwg_pt[TIME_PERIOD >= 1980]$REF_AREA)
coefci(reg_coupe_repetee,
       vcov = clustered_vcov)

#Figure

ggplot2::ggplot(data = gwg_pt,
                ggplot2::aes(x = ratio,
                             y = gwg,
                             color = REF_AREA,
                             group = REF_AREA)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line() +
  viridis::scale_color_viridis(option = "magma",
                               discrete = TRUE) + 
  ggplot2::theme_classic()

#FE regression

reg_panel <-
  lfe::felm(gwg ~ ratio | REF_AREA | 0 | REF_AREA,
            data = gwg_pt[TIME_PERIOD >= 1980])

#Student test
coeftest(reg_panel)

#Only 48 countries -> is that sufficient for asymptotic results to kick in?
#Not a random draw of countries obviously, which is the very foundation for our framework to estimate uncertainty...