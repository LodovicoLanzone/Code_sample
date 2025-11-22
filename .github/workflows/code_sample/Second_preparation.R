library (plm)
library(DescTools)
library(stargazer)
library(lfe)
library(dplyr)
library(survival)
library(lmtest)
library(sandwich)
library (fixest)
library(pscl)
library (TMB)
library (glmmTMB)
library (ggplot2)
library (rlang)
library (margins)
library(haven)
library(arrow)
library(data.table)
library(purrr)
library(gridExtra)
library(grid)
library(tibble)
setwd ("...")

dataprocessedpath <- "SUF_PROCESSED/"
outputpath <- "OUTPUT_SUF"

data_clean <- readRDS(paste0(dataprocessedpath, "first_clean_data.rds")) 

#small change before remaking the datasets with the small adjustments
data_clean <- data_clean %>% mutate(
  schultyp = case_when(
    year == 1978 & schultyp == 9 ~ NA_real_,
    TRUE ~ schultyp
  )
)
  

#further initial preparation
data_clean <- data_clean %>%
  filter (
    private_hh == 1
  ) %>%
  select ( -private_hh)
data_clean <- data_clean %>%
  mutate(jahresueberhang = if_else(is.na(jahresueberhang), 0, jahresueberhang))

#creation of unique identifier: 2 versions, 1 in which we account for the panel dimension, one in which we don't. Then, the rest of the code can remain unchanged
option = "1" #1 is accounting for panel dimension, 2 is not accounting for it
suboption = "1" #1 is using pnr constant, 2 is using pnr as sequence also after 2011, 3 is using given iid from 2020 and pnr constant for the rest

setDT(data_clean)
if (option == "1") {
  data_clean[year < 1989, hhid := .GRP, by = .(land, Auswahlbezirk, hhnr)]
  max_hhid <- data_clean[year < 1989, max(as.numeric(hhid), na.rm = TRUE)]
  data_clean[year == 1989, hhid := as.character(as.numeric(hhid) + max_hhid)]
  max_hhid <- data_clean[year < 1990, max(as.numeric(hhid), na.rm = TRUE)]
  data_clean[year > 1989, hhid := as.character(.GRP + max_hhid), by = .(land, Auswahlbezirk, jahresueberhang, hhnr)]
} else if (option == "2") {
  data_clean[year < 1989, hhid := .GRP, by = .(land, Auswahlbezirk, hhnr, year)]
  max_hhid <- data_clean[year < 1989, max(as.numeric(hhid), na.rm = TRUE)]
  data_clean[year == 1989, hhid := as.character(as.numeric(hhid) + max_hhid)]
  max_hhid <- data_clean[year < 1990, max(as.numeric(hhid), na.rm = TRUE)]
  data_clean[year > 1989, hhid := as.character(.GRP + max_hhid), by = .(land, Auswahlbezirk, jahresueberhang, hhnr, year)]
}
if (suboption == "1") {
  data_clean[year > 2010, 
             iid := .GRP, 
             by = .(hhid, pnr_constant)]
  max_iid <- data_clean[year > 2010, max(as.numeric(iid), na.rm = TRUE)]
  data_clean[year <= 2010, 
             pnr := seq_len(.N), 
             by = .(hhid, year)]
  data_clean[year <= 2010, 
             iid := as.character(.GRP + max_iid), 
             by = .(hhid, pnr)]
} else if (suboption == "2") {
  data_clean[ , pnr := seq_len(.N), 
             by = .(hhid, year)]
  data_clean[ , iid := .GRP, 
             by = .(hhid, pnr)]
} else if (suboption == "3") {
  max_iid <- data_clean[year > 2019, max(as.numeric(iid), na.rm = TRUE)]
  data_clean[year > 2010 & year < 2020, 
             iid := as.character(.GRP + max_iid), 
             by = .(hhid, pnr_constant)]
  max_iid <- data_clean[year > 2010, max(as.numeric(iid), na.rm = TRUE)]
  data_clean[year <= 2010, 
             pnr := seq_len(.N), 
             by = .(hhid, year)]
  data_clean[year <= 2010, 
             iid := as.character(.GRP + max_iid), 
             by = .(hhid, pnr)]
}
data_clean <- as.data.frame(data_clean)

#further cleaning
data_clean <- data_clean %>%
  mutate(inc_hh = case_when(
    inc_hh %in% c(50, 99) ~ NA_real_,
    TRUE ~ inc_hh
  ))
data_clean <- data_clean %>%
  mutate(inc_p = case_when(
    inc_per_capita %in% c(50, 90, 99) ~ NA_real_, 
    TRUE ~ inc_per_capita
  ))

#creation of family relation:
data_clean <- data_clean %>%
  mutate(child = if_else(pos_in_lebensform == 3 | pos_in_family == 3, 1L, 0L),
         parent = if_else (pos_in_lebensform %in% c(1,2) | pos_in_family %in% c(1,2), 1L, 0L))


#further cleaning of educational data
data_clean <-data_clean %>%
  mutate (
    degree_yn = ifelse(degree_yn == 9, NA, degree_yn),
    klassenstufe = case_when(
      klassenstufe %in% c(4, 9) ~ NA,
      TRUE ~ klassenstufe
    ),
    highest_degree = case_when(
      as.numeric(highest_degree) == 7 ~ 2,
      as.numeric(highest_degree) == 6 ~ 1,
      TRUE ~ as.numeric(highest_degree)
    )
  )

#creation of continuos income
income_classes_1973 <- data.frame(
  inc_p = 1:15,
  lower_bound = c(50, 150, 300, 450, 600, 800, 1000, 1200, 1400, 1600,
                  1800, 2000, 2200, 2500, 3000),
  upper_bound = c(149, 299, 449, 599, 799, 999, 1199, 1399, 1599, 1799,
                  1999, 2499, 2599, 2999, 3200)
)

income_classes_1978 <- data.frame(
  inc_p = 1:18,
  lower_bound = c(50, 300, 450, 600, 800, 1000, 1200, 1400, 1600,
                  1800, 2000, 2200, 2500, 3000, 3500, 4000, 4500, 5000),
  upper_bound = c(299, 449, 599, 799, 999, 1199, 1399, 1599, 1799,
                  1999, 2499, 2599, 2999, 3499, 3999, 4499, 4999, 5200)
)

income_classes_2008 <- data.frame(
  inc_p = 1:24,
  lower_bound = c(50, 150, 300, 500, 700, 900, 1100, 1300, 1500, 1700, 2000, 2300,
                  2600, 2900, 3200, 3600, 4000, 4500, 5000, 5500, 6000, 7500, 10000, 18000),
  upper_bound = c(149, 299, 499, 699, 899, 1099, 1299, 1499, 1699, 1999, 2299, 2599,
                  2899, 3199, 3599, 3999, 4499, 4999, 5499, 5999, 7499, 9999, 17999, 18200)
)

income_classes_2020 <- data.frame(
  inc_p = 1:24,
  lower_bound = c(50, 250, 500, 750, 1000, 1250, 1500, 1750,
                  2000, 2250, 2500, 2750, 3000, 3250, 3500,
                  4000, 4500, 5000, 6000, 7000, 8000,
                  10000, 15000, 25000),
  upper_bound = c(249, 499, 749, 999, 1249, 1499, 1749, 1999,
                  2249, 2499, 2749, 2999, 3249, 3499, 3999,
                  4499, 4999, 5999, 6999, 7999, 9999,
                  14999, 24999, 25200)
)
set.seed(999)
random_sequence <- sample(0:99, 1000, replace = TRUE)
# function that applies the transformation given a dataset and income class table
transform_income <- function(df, income_classes) {
  df %>%
    left_join(income_classes, by = "inc_p") %>%
    mutate(
      Z_k = random_sequence[(row_number() - 1) %% 1000 + 1],
      B_i = (upper_bound - lower_bound) / 100,
      inc_p_hilf = lower_bound + B_i * Z_k
    )
}
# apply depending on year
data_clean<- bind_rows(
  data_clean %>% filter(year >= 2020) %>% transform_income(income_classes_2020),
  data_clean %>% filter(year >= 2008 & year < 2020) %>% transform_income(income_classes_2008),
  data_clean %>% filter(year >= 1978 & year < 2008) %>% transform_income(income_classes_1978),
  data_clean %>% filter(year >= 1973 & year < 1978) %>% transform_income(income_classes_1973)
)

#create household income continuous:
household_income <- data_clean %>%
  group_by(hhid, year) %>%
  summarise(inc_hh_hilf = sum(inc_p_hilf, na.rm = TRUE), .groups = "drop")
data_clean <- data_clean %>%
  left_join(household_income, by = c("hhid", "year"))


cpi_values <- tibble(
  year = c (1973, 1976, 1978, 1980, 1982, 1985, 1987, 1989, 2008:2022),
  cpi = c(0.668, 0.789, 0.840, 0.922, 1.032, 1.114, 1.115, 1.161, 0.869, 0.872, 0.881, 0.900, 0.917, 0.931, 0.940, 0.945, 0.950,
          0.964, 0.981, 0.995, 1.000, 1.031, 1.102)
)
data_clean <- data_clean %>%
  mutate(across(c(inc_hh_hilf, inc_p_hilf), ~ na_if(., 99999))) %>%
  left_join(cpi_values, by = "year") %>%
  { if (any(is.na(.$cpi))) stop("Missing CPI values") else . } %>%
  mutate(
    inc_hh_hilf = inc_hh_hilf / cpi,
    inc_p_hilf = inc_p_hilf / cpi
  )
#for oecd adjustment factor
setDT(data_clean)
data_clean[, total_child_inc := sum(inc_p_hilf * (child == 1), na.rm = TRUE),
           by = .(hhid, year)]
data_clean[, `:=`(
  n_par      = sum(parent == 1, na.rm = TRUE),
  n_children = sum(child == 1, na.rm = TRUE)
), by = .(hhid, year)]
data_clean[, oecd := fifelse(
  n_par > 0,
  1 + 0.5 * (n_par - 1) + 0.3 * n_children,
  NA_real_
)]

data_clean <- data_clean[n_par <= 2]
data_clean[, inc_par := pmax(inc_hh_hilf - total_child_inc, 0)]
data_clean[, inc_hh  := inc_par / oecd]


#create gym dummy
data_clean <- data_clean[,gym := fcase(
  schultyp ==3 , 1,
  schultyp != 3 & schultyp != 50  & !is.na (schultyp) , 0,
  default = NA_real_)]

#abitur dummy
data_clean[, outcome3 := 0L]
data_clean[year > 1989 & (highest_degree %in% c(4, 5) |
                            ontrack_allg == 1 | ontrack_berufl == 1), outcome3 := 1L]
data_clean[year > 1989 & is.na(highest_degree) & is.na(ontrack_allg) & is.na(ontrack_berufl),
           outcome3 := NA_integer_]
data_clean[year <= 1989 & year > 1973 & (highest_degree %in% c(4, 5) | schultyp %in% c(3, 8)),
           outcome3 := 1L]

data_clean[year <= 1989 & year > 1973 & is.na(highest_degree) & is.na(schultyp),
           outcome3 := NA_integer_]

saveRDS(data_clean, file = paste0(dataprocessedpath, "second_clean_data_abitur_test.rds"))



