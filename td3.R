######################################################################################
######################################################################################
#EXERCISES
######################################################################################
######################################################################################


#Exercise 1

#Using data.table


dt <- data.table(x = 1:10)
dt[x %% 2 == 0]

#Using the tidyverse

df <- tibble(x = 1:10)
df %>% filter(x %% 2 == 0)

#Exercise 2

#Using data.table

dt <- data.table(x = 1:5)
dt[, carre := x^2]
dt

#Using the tidyverse

df <- tibble(x = 1:5)
df %>% mutate(carre = x^2)


#Exercise 3

#Using data.table

dt <- data.table(groupe = c("A","A","B","B","B"),
                 valeur = c(1,2,3,4,5))

dt[, .(moyenne = mean(valeur)), by = groupe]

#Using the tidyverse

df <- tibble(groupe = c("A","A","B","B","B"),
             valeur = c(1,2,3,4,5))

df %>% group_by(groupe) %>%
  summarise(moyenne = mean(valeur))

#Exercise 4

#Using data.table

dt <- data.table(id = c(1,1,2,2),
                 carburant = c("SP95","Gazole","SP95","Gazole"),
                 prix = c(1.5,1.7,1.6,1.8))

dcast(dt, id ~ carburant, value.var = "prix")

#Using the tidyverse

df <- tibble(id = c(1,1,2,2),
             carburant = c("SP95","Gazole","SP95","Gazole"),
             prix = c(1.5,1.7,1.6,1.8))

df %>% pivot_wider(names_from = carburant, values_from = prix)

#Exercise 5

#Using data.table

stations <- data.table(id = c(1,2), ville = c("Paris","Lyon"))
prix <- data.table(id = c(1,2), Gazole = c(1.7,1.8))

merge(stations, prix, by = "id")

#Using the tidyverse

stations <- tibble(id = c(1,2), ville = c("Paris","Lyon"))
prix <- tibble(id = c(1,2), Gazole = c(1.7,1.8))

stations %>% left_join(prix, by = "id")

#Exercise 6

#USing data.table

dt <- data.table(id = c(1,2),
                 SP95 = c(1.5,1.6),
                 Gazole = c(1.7,1.8))

melt(dt, id.vars = "id", variable.name = "carburant", value.name = "prix")

#Using the tidyverse

df <- tibble(id = c(1,2),
             SP95 = c(1.5,1.6),
             Gazole = c(1.7,1.8))

df %>% pivot_longer(cols = c(SP95, Gazole),
                    names_to = "carburant",
                    values_to = "prix")

#Exercise 7

#Using data.table

dt <- data.table(id = 1:3,
                 a = 1:3,
                 b = 4:6,
                 c = 7:9)

dt[, row_mean := rowMeans(.SD), .SDcols = c("a","b","c")]

#Using the tidyverse

df <- tibble(id = 1:3,
             a = 1:3,
             b = 4:6,
             c = 7:9)

df %>% rowwise() %>%
  mutate(row_mean = mean(c_across(a:c))) %>%
  ungroup()

#Exercise 8

#Using data.table

dt <- data.table(store   = c("A","A","B","B"),
                 product = c("apple","orange","apple","orange"),
                 sales   = c(10,5,8,7),
                 price   = c(1.2,1.5,1.1,1.4))

dt[, unlist(lapply(.SD, function(x) list(mean = mean(x), sd = sd(x))), recursive = FALSE),
   by = store, .SDcols = c("sales", "price")]

#Using the tidyverse
df <- tibble(store   = c("A","A","B","B"),
             product = c("apple","orange","apple","orange"),
             sales   = c(10,5,8,7),
             price   = c(1.2,1.5,1.1,1.4))

df %>%
  group_by(store) %>%
  summarise(across(c(sales, price),
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}"))

######################################################################################
######################################################################################
#PROBLEM
######################################################################################
######################################################################################


######################################################################################
# Using data.table
######################################################################################

# Load the data

prix_carburant <- 
  data.table::fread("./prix-carburants-quotidien.csv")

# Normalise columns names

normalise_string <- function(string) {
  
  string <- tolower(string)
  string <- stringi::stri_trans_general(str = string,
                                        id = "Latin-ASCII")
  string <- gsub(pattern = "[[:punct:]]",
                 replacement = "",
                 x = string)
  string <- gsub(pattern = "[[:blank:]]",
                 replacement = "_",
                 x = string)
  
} 

colnames(prix_carburant) <- normalise_string(colnames(prix_carburant))

# Create stations table

columns_to_keep <-
  c("id",
    "Code postal",
    "adresse",
    "com_arm_code",
    "Commune / Arrondissement municipal",
    "Numéro Département",
    "Département",
    "Code officiel Région",
    "Région",
    "geom")

columns_to_keep <- normalise_string(columns_to_keep)

stations <-
  unique(prix_carburant[,..columns_to_keep])


#Define latitude and longitude

stations[,
         c("lat",
           "long") :=
           data.table::tstrsplit(x = geom,
                                 split = ",",
                                 fixed = TRUE)]

stations[,
         c("long",
           "lat") := 
           lapply(X = .SD,
                FUN = as.numeric),
         .SDcols = c("long",
                     "lat")]

plot(stations[,c("long","lat")])

#Create price table

prices <-
  data.table::dcast(
    data = prix_carburant[!is.na(prix)],
    id ~ carburant,
    value.var = "prix",
    fun.aggregate = as.numeric,
    fill = NA_real_
  )

#Fusion des deux tables

price_station <-
  merge(
    x = stations,
    y = prices,
    by = "id",
    all.x = TRUE,
    all.y = TRUE)
  
#Compute average prices and average longitude and latitude

columns_gastype <-
  colnames(prices)[colnames(prices) != "id"]

average_prices <-
  price_station[,
               c(lat_moy = mean(lat),
                 long_moy = mean(long),
                 unlist(
                   lapply(X = .SD,
                          FUN = function(var_price)
                            list(avg = mean(var_price, na.rm = TRUE),
                                 q25 =  quantile(var_price, probs = 0.25, na.rm = TRUE),
                                 median = quantile(var_price, probs = 0.5, na.rm = TRUE),
                                 q75= quantile(var_price, probs = 0.75, na.rm = TRUE))),
                   recursive = FALSE)),
               .SDcols = columns_gastype,
               by = c("departement",
                      "numero_departement")]

#Représentation graphique

ggplot2::ggplot(data = average_prices,
                ggplot2::aes(x = long_moy,
                             y = lat_moy,
                             color = Gazole.avg)) +
  ggplot2::theme_classic() +
  ggplot2::geom_point(size = 5) +
  ggplot2::scale_color_viridis_c(option = "magma",
                             name = "Average gasoil price\n (in euros)",
                             direction = -1,
                             begin = 0.1,
                             end = 0.9)

#Flag large deviation

threshold = 0.1

price_station[,
             sp98_gap_avg_price_over_threshold :=
               as.numeric(abs(SP98 - mean(SP98, na.rm = TRUE)) / mean(SP98, na.rm = TRUE) >= threshold),
             by = c("departement")]
  
large_dev <-
  price_station[,
               sum(sp98_gap_avg_price_over_threshold, na.rm = TRUE),
               by = c("departement")]

large_dev

########################################################################################
#Using the tidyverse
########################################################################################

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Load the data
prix_carburant <- read_delim("./prix-carburants-quotidien.csv",
                           delim = ";")

# Normalise columns names
normalise_string <- function(string) {
  string %>%
    str_to_lower() %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[[:blank:]]", "_")
}

colnames(prix_carburant) <- normalise_string(colnames(prix_carburant))

# Create station table
columns_to_keep <- c("id",
                        "Code postal",
                        "adresse",
                        "com_arm_code",
                        "Commune / Arrondissement municipal",
                        "Numéro Département",
                        "Département",
                        "Code officiel Région",
                        "Région",
                        "geom") %>%
  normalise_string()

stations <- prix_carburant %>%
  select(all_of(columns_to_keep)) %>%
  distinct()

# Define latitude and longitude
stations <- stations %>%
  separate(geom, into = c("lat", "long"), sep = ",", convert = TRUE)

# Raw plot
plot(stations$long, stations$lat)

# Create price table
prices <- prix_carburant %>%
  filter(!is.na(prix)) %>%
  pivot_wider(
    id_cols = id,
    names_from = carburant,
    values_from = prix,
    values_fn = mean,            # ou median, ou function(x) tail(x,1)
    values_fill = NA_real_
  )


# Joint
price_station <- stations %>%
  left_join(prices, by = "id")

# Compute average prices and average coordinates
columns_gastype <- setdiff(names(prices), "id")

average_prices <- price_station %>%
  group_by(departement, numero_departement) %>%
  summarise(
    lat_moy = mean(lat, na.rm = TRUE),
    long_moy = mean(long, na.rm = TRUE),
    across(
      all_of(columns_gastype),
      list(
        avg = ~mean(.x, na.rm = TRUE),
        q25 = ~quantile(.x, probs = 0.25, na.rm = TRUE),
        median = ~quantile(.x, probs = 0.5, na.rm = TRUE),
        q75 = ~quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    ),
    .groups = "drop"
  )

# Représentation graphique
ggplot(average_prices,
       aes(x = long_moy,
           y = lat_moy,
           color = Gazole.avg)) +
  geom_point(size = 5) +
  scale_color_viridis_c(
    option = "magma",
    name = "Average gasoil price\nin euros",
    direction = -1,
    begin = 0.1,
    end = 0.9
  ) +
  theme_classic()

# Flag large deviations
threshold <- 0.1

price_station <- price_station %>%
  group_by(departement) %>%
  mutate(
    sp98_gap_avg_price_over_threshold =
      as.numeric(abs(SP98 - mean(SP98, na.rm = TRUE)) /
                   mean(SP98, na.rm = TRUE) >= threshold)
  ) %>%
  ungroup()

large_dev <- price_station %>%
  group_by(departement) %>%
  summarise(nb_dev = sum(sp98_gap_avg_price_over_threshold, na.rm = TRUE))
