install.packages("devtools")
devtools::install_github("world-inequality-database/gpinter")
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
library(tidyr)
library(readr)
library(gpinter)
library(purrr)
library(ineq)
rm(list = ls())
setwd("...")

#LOADING DATASETS
path00 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2000.csv'
data00 = read.csv2(path00, fileEncoding = "latin1")
path01 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2001.csv'
data01 = read.csv2(path01, fileEncoding = "latin1")
path02 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2002.csv'
data02 = read.csv2(path02, fileEncoding = "latin1")
path03 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2003.csv'
data03 = read.csv2(path03, fileEncoding = "latin1")
path04 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2004.csv'
data04 = read.csv2(path04, fileEncoding = "latin1")
path05 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2005.csv'
data05 = read.csv2(path05, fileEncoding = "latin1")
path06 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2006.csv'
data06 = read.csv2(path06, fileEncoding = "latin1")
path07 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2007.csv'
data07 = read.csv2(path07, fileEncoding = "latin1")
path08 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2008.csv'
data08 = read.csv2(path08, fileEncoding = "latin1")
path09 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2009.csv'
data09 = read.csv2(path09, fileEncoding = "latin1")
path10 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2010.csv'
data10 = read.csv2(path10, fileEncoding = "latin1")
path11 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2011.csv'
data11 = read.csv2(path11, fileEncoding = "latin1")
path12 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2012.csv'
data12 = read.csv2(path12, fileEncoding = "latin1")
path13 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2013.csv'
data13 = read.csv2(path13, fileEncoding = "latin1")
path14 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2014.csv'
data14 = read.csv2(path14, fileEncoding = "latin1")
path15 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2015.csv'
data15 = read.csv2(path15, fileEncoding = "latin1")
path16 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2016.csv'
data16 = read.csv2(path16, fileEncoding = "latin1")
path17 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2017.csv'
data17 = read.csv2(path17, fileEncoding = "latin1")
path18 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2018.csv'
data18 = read.csv2(path18, fileEncoding = "latin1")
path19 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2019.csv'
data19 = read_csv2(path19)
path20 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.csv'
data20 = read_csv2(path20)
path21 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2021.csv'
data21 = read_csv2(path21)
path22 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2022.csv'
data22 <- read_csv2(path22, locale = locale(encoding = "Latin1"))
path23 = 'raw_data/inequality_data/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2023.csv'
data23 = read.csv2(path23, fileEncoding = "latin1", row.names = NULL)



#cleaning datasets: done both excluding special autonomy regions and keeping it. 
#Keeping only to create maps. So, to get both datasets the following code should be run twice
cleaning <- function(data) {
  data %>%
    rename(
      income_less_0_freq = "Reddito.complessivo.minore.o.uguale.a.zero.euro...Frequenza",
      income_less_0_tot = "Reddito.complessivo.minore.o.uguale.a.zero.euro...Ammontare",
      income_0_10_freq = "Reddito.complessivo.da.0.a.10000.euro...Frequenza",
      income_0_10_tot = "Reddito.complessivo.da.0.a.10000.euro...Ammontare",
      income_10_15_freq = "Reddito.complessivo.da.10000.a.15000.euro...Frequenza",
      income_10_15_tot = "Reddito.complessivo.da.10000.a.15000.euro...Ammontare",
      income_15_26_freq = "Reddito.complessivo.da.15000.a.26000.euro...Frequenza",
      income_15_26_tot = "Reddito.complessivo.da.15000.a.26000.euro...Ammontare",
      income_26_55_freq = "Reddito.complessivo.da.26000.a.55000.euro...Frequenza",
      income_26_55_tot = "Reddito.complessivo.da.26000.a.55000.euro...Ammontare",
      income_55_75_freq = "Reddito.complessivo.da.55000.a.75000.euro...Frequenza",
      income_55_75_tot = "Reddito.complessivo.da.55000.a.75000.euro...Ammontare",
      income_75_120_freq = "Reddito.complessivo.da.75000.a.120000.euro...Frequenza",
      income_75_120_tot = "Reddito.complessivo.da.75000.a.120000.euro...Ammontare",
      income_more_120_freq = "Reddito.complessivo.oltre.120000.euro...Frequenza",
      income_more_120_tot = "Reddito.complessivo.oltre.120000.euro...Ammontare",
      year = "Anno.di.imposta",
      municipality = "Denominazione.Comune",
      province = "Sigla.Provincia",
      region = "Regione",
      ISTAT = "Codice.Istat.Comune",
      Code = Codice.catastale
    ) %>%
    select(
      year, Code, ISTAT, municipality, province, region,
      income_less_0_freq, income_less_0_tot,
      income_0_10_freq, income_0_10_tot,
      income_10_15_freq, income_10_15_tot,
      income_15_26_freq, income_15_26_tot,
      income_26_55_freq, income_26_55_tot,
      income_55_75_freq, income_55_75_tot,
      income_75_120_freq, income_75_120_tot,
      income_more_120_freq, income_more_120_tot
    ) %>%
  #  filter(
   #   !region %in% c(
    #    "VALLE D AOSTA", "VALLE D'AOSTA", "Valle d'Aosta", 
     #   "TRENTINO ALTO ADIGE", "PROV. AUTONOMA BOLZANO", "PROV. AUTONOMA TRENTO", "Trentino Alto Adige", "Trentino Alto Adige(P.A.Trento)",
      #  "FRIULI VENEZIA GIULIA", "FRIULIVENEZIA GIULIA", "FRIULI-VENEZIA GIULIA", "Friuli Venezia Giulia",
    #    "SARDEGNA", "Sardegna",
    #     "SICILIA","Sicilia",
    #     "REGIONE ASSENTE", "Non indicato"
    #   )
    # ) %>%
    mutate (
      region = if_else (region == "Veneto", "VENETO", region),
      region = if_else (region == "Lombardia", "LOMBARDIA", region),
      region = if_else (region == "Toscana", "TOSCANA", region),
      region = if_else (region == "Abruzzo", "ABRUZZO", region),
      region = if_else (region == "Basilicata", "BASILICATA", region),
      region = if_else (region == "Puglia", "PUGLIA", region),
      region = if_else (region == "Piemonte", "PIEMONTE", region),
      region = if_else (region == "Lazio", "LAZIO", region),
      region = if_else (region == "Campania", "CAMPANIA", region),
      region = if_else (region == "Marche", "MARCHE", region),
      region = if_else (region == "Calabria", "CALABRIA", region),
      region = if_else (region == "Umbria", "UMBRIA", region),
      region = if_else (region == "Molise", "MOLISE", region),
      region = if_else (region == "Liguria", "LIGURIA", region),
      region = if_else (region %in% c ("EMILIAROMAGNA", "EMILIA-ROMAGNA"), "EMILIA ROMAGNA", region)
    )
}

cleaning2 <- function(data) {
  data %>%
    rename(
      income_less_0_freq = Reddito.complessivo.minore.o.uguale.a.zero.euro...Frequenza,
      income_less_0_tot = Reddito.complessivo.minore.o.uguale.a.zero.euro...Ammontare.in.euro,
      income_0_10_freq = Reddito.complessivo.da.0.a.10000.euro...Frequenza,
      income_0_10_tot = Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro,
      income_10_15_freq = Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
      income_10_15_tot = Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro,
      income_15_26_freq = Reddito.complessivo.da.15000.a.26000.euro...Frequenza,
      income_15_26_tot = Reddito.complessivo.da.15000.a.26000.euro...Ammontare.in.euro,
      income_26_55_freq = Reddito.complessivo.da.26000.a.55000.euro...Frequenza,
      income_26_55_tot = Reddito.complessivo.da.26000.a.55000.euro...Ammontare.in.euro,
      income_55_75_freq = Reddito.complessivo.da.55000.a.75000.euro...Frequenza,
      income_55_75_tot = Reddito.complessivo.da.55000.a.75000.euro...Ammontare.in.euro,
      income_75_120_freq = Reddito.complessivo.da.75000.a.120000.euro...Frequenza,
      income_75_120_tot = Reddito.complessivo.da.75000.a.120000.euro...Ammontare.in.euro,
      income_more_120_freq = Reddito.complessivo.oltre.120000.euro...Frequenza,
      income_more_120_tot = Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
      year = Anno.di.imposta,
      municipality = Denominazione.Comune,
      province = Sigla.Provincia,
      region = Regione,
      ISTAT = Codice.Istat.Comune,
      Code = Codice.catastale
    ) %>%
    select(
      year, Code, ISTAT, municipality, province, region,
      income_less_0_freq, income_less_0_tot,
      income_0_10_freq, income_0_10_tot,
      income_10_15_freq, income_10_15_tot,
      income_15_26_freq, income_15_26_tot,
      income_26_55_freq, income_26_55_tot,
      income_55_75_freq, income_55_75_tot,
      income_75_120_freq, income_75_120_tot,
      income_more_120_freq, income_more_120_tot
    ) #%>%
  #  filter(
  #    !region %in% c(
  #      "VALLE D AOSTA", "VALLE D'AOSTA", "Valle d'Aosta", 
  #      "TRENTINO ALTO ADIGE", "PROV. AUTONOMA BOLZANO", "PROV. AUTONOMA TRENTO", "Trentino Alto Adige", "Trentino Alto Adige(P.A.Trento)",
  #      "FRIULI VENEZIA GIULIA", "FRIULIVENEZIA GIULIA", "FRIULI-VENEZIA GIULIA", "Friuli Venezia Giulia",
  #      "SARDEGNA", "Sardegna",
  #      "SICILIA","Sicilia",
  #      "REGIONE ASSENTE", "Non indicato"
  #    )
  #  ) 
}

cleaning3 <- function(data) {
  data %>%
    rename(
      income_less_0_freq = "Reddito.complessivo.minore.o.uguale.a.zero.euro...Frequenza",
      income_less_0_tot = "Reddito.complessivo.minore.o.uguale.a.zero.euro...Ammontare",
      income_0_10_freq = "Reddito.complessivo.da.0.a.10000.euro...Frequenza",
      income_0_10_tot = "Reddito.complessivo.da.0.a.10000.euro...Ammontare",
      income_10_15_freq = "Reddito.complessivo.da.10000.a.15000.euro...Frequenza",
      income_10_15_tot = "Reddito.complessivo.da.10000.a.15000.euro...Ammontare",
      income_15_26_freq = "Reddito.complessivo.da.15000.a.26000.euro...Frequenza",
      income_15_26_tot = "Reddito.complessivo.da.15000.a.26000.euro...Ammontare",
      income_26_55_freq = "Reddito.complessivo.da.26000.a.55000.euro...Frequenza",
      income_26_55_tot = "Reddito.complessivo.da.26000.a.55000.euro...Ammontare",
      income_55_75_freq = "Reddito.complessivo.da.55000.a.75000.euro...Frequenza",
      income_55_75_tot = "Reddito.complessivo.da.55000.a.75000.euro...Ammontare",
      income_75_120_freq = "Reddito.complessivo.da.75000.a.120000.euro...Frequenza",
      income_75_120_tot = "Reddito.complessivo.da.75000.a.120000.euro...Ammontare",
      income_more_120_freq = "Reddito.complessivo.oltre.120000.euro...Frequenza",
      income_more_120_tot = "Reddito.complessivo.oltre.120000.euro...Ammontare",
      year = "Anno.di.imposta",
      municipality = "Denominazione.Comune",
      province = "Sigla.Provincia",
      region = "Regione",
      ISTAT = "Codice.Istat",
      Code = Codice.catastale
    ) %>%
    select(
      year, Code, ISTAT, municipality, province, region,
      income_less_0_freq, income_less_0_tot,
      income_0_10_freq, income_0_10_tot,
      income_10_15_freq, income_10_15_tot,
      income_15_26_freq, income_15_26_tot,
      income_26_55_freq, income_26_55_tot,
      income_55_75_freq, income_55_75_tot,
      income_75_120_freq, income_75_120_tot,
      income_more_120_freq, income_more_120_tot
    ) %>%
    #  filter(
    #  !region %in% c(
    #    "VALLE D AOSTA", "VALLE D'AOSTA", "Valle d'Aosta", 
    #    "TRENTINO ALTO ADIGE", "PROV. AUTONOMA BOLZANO", "PROV. AUTONOMA TRENTO", "Trentino Alto Adige", "Trentino Alto Adige(P.A.Trento)",
    #    "FRIULI VENEZIA GIULIA", "FRIULIVENEZIA GIULIA", "FRIULI-VENEZIA GIULIA", "Friuli Venezia Giulia",
    #    "SARDEGNA", "Sardegna",
    #    "SICILIA","Sicilia",
    #    "REGIONE ASSENTE", "Non indicato"
    #  )
    #  ) %>%
    mutate (
      region = if_else (region == "Veneto", "VENETO", region),
      region = if_else (region == "Lombardia", "LOMBARDIA", region),
      region = if_else (region == "Toscana", "TOSCANA", region),
      region = if_else (region == "Abruzzo", "ABRUZZO", region),
      region = if_else (region == "Basilicata", "BASILICATA", region),
      region = if_else (region == "Puglia", "PUGLIA", region),
      region = if_else (region == "Piemonte", "PIEMONTE", region),
      region = if_else (region == "Lazio", "LAZIO", region),
      region = if_else (region == "Campania", "CAMPANIA", region),
      region = if_else (region == "Marche", "MARCHE", region),
      region = if_else (region == "Calabria", "CALABRIA", region),
      region = if_else (region == "Umbria", "UMBRIA", region),
      region = if_else (region == "Molise", "MOLISE", region),
      region = if_else (region == "Liguria", "LIGURIA", region),
      region = if_else (region %in% c ("EMILIAROMAGNA", "EMILIA-ROMAGNA"), "EMILIA ROMAGNA", region)
    )
}

cleaning4 <- function(data) {
  data %>%
    rename(
      income_less_0_freq = "Reddito.complessivo.minore.di.zero.euro...Frequenza",
      income_less_0_tot = "Reddito.complessivo.minore.di.zero.euro...Ammontare",
      income_0_10_freq = "Reddito.complessivo.da.0.a.10000.euro...Frequenza",
      income_0_10_tot = "Reddito.complessivo.da.0.a.10000.euro...Ammontare",
      income_10_15_freq = "Reddito.complessivo.da.10000.a.15000.euro...Frequenza",
      income_10_15_tot = "Reddito.complessivo.da.10000.a.15000.euro...Ammontare",
      income_15_26_freq = "Reddito.complessivo.da.15000.a.26000.euro...Frequenza",
      income_15_26_tot = "Reddito.complessivo.da.15000.a.26000.euro...Ammontare",
      income_26_55_freq = "Reddito.complessivo.da.26000.a.55000.euro...Frequenza",
      income_26_55_tot = "Reddito.complessivo.da.26000.a.55000.euro...Ammontare",
      income_55_75_freq = "Reddito.complessivo.da.55000.a.75000.euro...Frequenza",
      income_55_75_tot = "Reddito.complessivo.da.55000.a.75000.euro...Ammontare",
      income_75_120_freq = "Reddito.complessivo.da.75000.a.120000.euro...Frequenza",
      income_75_120_tot = "Reddito.complessivo.da.75000.a.120000.euro...Ammontare",
      income_more_120_freq = "Reddito.complessivo.oltre.120000.euro...Frequenza",
      income_more_120_tot = "Reddito.complessivo.oltre.120000.euro...Ammontare",
      year = "Anno.di.imposta",
      municipality = "Denominazione.Comune",
      province = "Sigla.Provincia",
      region = "Regione",
      ISTAT = "Codice.Istat.Comune",
      Code = Codice.catastale
    ) %>%
    select(
      year, Code, ISTAT, municipality, province, region,
      income_less_0_freq, income_less_0_tot,
      income_0_10_freq, income_0_10_tot,
      income_10_15_freq, income_10_15_tot,
      income_15_26_freq, income_15_26_tot,
      income_26_55_freq, income_26_55_tot,
      income_55_75_freq, income_55_75_tot,
      income_75_120_freq, income_75_120_tot,
      income_more_120_freq, income_more_120_tot
    ) %>%
    #  filter(
    #   !region %in% c(
    #     "VALLE D AOSTA", "VALLE D'AOSTA", "Valle d'Aosta", 
    #     "TRENTINO ALTO ADIGE", "PROV. AUTONOMA BOLZANO", "PROV. AUTONOMA TRENTO", "Trentino Alto Adige", "Trentino Alto Adige(P.A.Trento)",
    #    "FRIULI VENEZIA GIULIA", "FRIULIVENEZIA GIULIA", "FRIULI-VENEZIA GIULIA", "Friuli Venezia Giulia",
    #    "SARDEGNA", "Sardegna",
    #    "SICILIA","Sicilia",
    #    "REGIONE ASSENTE", "Non indicato"
    #  )
    # ) %>%
    mutate (
      region = if_else (region == "Veneto", "VENETO", region),
      region = if_else (region == "Lombardia", "LOMBARDIA", region),
      region = if_else (region == "Toscana", "TOSCANA", region),
      region = if_else (region == "Abruzzo", "ABRUZZO", region),
      region = if_else (region == "Basilicata", "BASILICATA", region),
      region = if_else (region == "Puglia", "PUGLIA", region),
      region = if_else (region == "Piemonte", "PIEMONTE", region),
      region = if_else (region == "Lazio", "LAZIO", region),
      region = if_else (region == "Campania", "CAMPANIA", region),
      region = if_else (region == "Marche", "MARCHE", region),
      region = if_else (region == "Calabria", "CALABRIA", region),
      region = if_else (region == "Umbria", "UMBRIA", region),
      region = if_else (region == "Molise", "MOLISE", region),
      region = if_else (region == "Liguria", "LIGURIA", region),
      region = if_else (region %in% c ("EMILIAROMAGNA", "EMILIA-ROMAGNA"), "EMILIA ROMAGNA", region)
    )
}
cleaning5 <- function(data) {
  data %>%
    rename(
      income_less_0_freq = "Reddito complessivo minore o uguale a zero euro - Frequenza",
      income_less_0_tot = "Reddito complessivo minore o uguale a zero euro - Ammontare in euro",
      income_0_10_freq = "Reddito complessivo da 0 a 10000 euro - Frequenza",
      income_0_10_tot = "Reddito complessivo da 0 a 10000 euro - Ammontare in euro",
      income_10_15_freq = "Reddito complessivo da 10000 a 15000 euro - Frequenza",
      income_10_15_tot = "Reddito complessivo da 10000 a 15000 euro - Ammontare in euro",
      income_15_26_freq = "Reddito complessivo da 15000 a 26000 euro - Frequenza",
      income_15_26_tot = "Reddito complessivo da 15000 a 26000 euro - Ammontare in euro",
      income_26_55_freq = "Reddito complessivo da 26000 a 55000 euro - Frequenza",
      income_26_55_tot = "Reddito complessivo da 26000 a 55000 euro - Ammontare in euro",
      income_55_75_freq = "Reddito complessivo da 55000 a 75000 euro - Frequenza",
      income_55_75_tot = "Reddito complessivo da 55000 a 75000 euro - Ammontare in euro",
      income_75_120_freq = "Reddito complessivo da 75000 a 120000 euro - Frequenza",
      income_75_120_tot = "Reddito complessivo da 75000 a 120000 euro - Ammontare in euro",
      income_more_120_freq = "Reddito complessivo oltre 120000 euro - Frequenza",
      income_more_120_tot = "Reddito complessivo oltre 120000 euro - Ammontare in euro",
      year = "Anno di imposta",
      municipality = "Denominazione Comune" ,
      province = "Sigla Provincia",
      region = "Regione",
      ISTAT = "Codice Istat Comune",
      Code = "Codice catastale" 
    ) %>%
    select(
      year, Code, ISTAT, municipality, province, region,
      income_less_0_freq, income_less_0_tot,
      income_0_10_freq, income_0_10_tot,
      income_10_15_freq, income_10_15_tot,
      income_15_26_freq, income_15_26_tot,
      income_26_55_freq, income_26_55_tot,
      income_55_75_freq, income_55_75_tot,
      income_75_120_freq, income_75_120_tot,
      income_more_120_freq, income_more_120_tot
    ) %>%
    #  filter(
    #   !region %in% c(
    ##     "VALLE D AOSTA", "VALLE D'AOSTA", "Valle d'Aosta", 
    #    "TRENTINO ALTO ADIGE", "PROV. AUTONOMA BOLZANO", "PROV. AUTONOMA TRENTO", "Trentino Alto Adige", "Trentino Alto Adige(P.A.Trento)",
    #    "FRIULI VENEZIA GIULIA", "FRIULIVENEZIA GIULIA", "FRIULI-VENEZIA GIULIA", "Friuli Venezia Giulia",
    #    "SARDEGNA", "Sardegna",
    #    "SICILIA","Sicilia",
    #    "REGIONE ASSENTE", "Non indicato"
    #  )
    #  ) %>%
    mutate (
      region = if_else (region == "Veneto", "VENETO", region),
      region = if_else (region == "Lombardia", "LOMBARDIA", region),
      region = if_else (region == "Toscana", "TOSCANA", region),
      region = if_else (region == "Abruzzo", "ABRUZZO", region),
      region = if_else (region == "Basilicata", "BASILICATA", region),
      region = if_else (region == "Puglia", "PUGLIA", region),
      region = if_else (region == "Piemonte", "PIEMONTE", region),
      region = if_else (region == "Lazio", "LAZIO", region),
      region = if_else (region == "Campania", "CAMPANIA", region),
      region = if_else (region == "Marche", "MARCHE", region),
      region = if_else (region == "Calabria", "CALABRIA", region),
      region = if_else (region == "Umbria", "UMBRIA", region),
      region = if_else (region == "Molise", "MOLISE", region),
      region = if_else (region == "Liguria", "LIGURIA", region),
      region = if_else (region %in% c ("EMILIAROMAGNA", "EMILIA-ROMAGNA"), "EMILIA ROMAGNA", region)
    )
}
data00 <- cleaning2 (data00)
data01 <- cleaning2 (data01)
data02 <- cleaning (data02)
data03 <- cleaning (data03)
data04 <- cleaning (data04)
data05 <- cleaning (data05)
data06 <- cleaning (data06)
data07 <- cleaning (data07)
data08 <- cleaning3 (data08)
data09 <- cleaning3 (data09)
data10 <- cleaning3 (data10)
data11 <- cleaning3 (data11)
data12 <- cleaning (data12)
data13 <- cleaning (data13)
data14 <- cleaning4 (data14)
data15 <- cleaning2 (data15)
data16 <- cleaning2 (data16)
data17 <- cleaning2 (data17)
data18 <- cleaning2 (data18)
data19 <- cleaning5 (data19)
data20 <- cleaning5 (data20)
data21 <- cleaning5 (data21)
data22 <- cleaning5 (data22)
data23 <- cleaning2 (data23)

#adjusting dara types
data19$year <- as.integer(data19$year)
data20$year <- as.integer(data20$year)
data21$year <- as.integer(data21$year)
data22$year <- as.integer(data22$year)
data19$ISTAT <- as.integer(data19$ISTAT)
data20$ISTAT <- as.integer(data20$ISTAT)
data21$ISTAT <- as.integer(data21$ISTAT)
data22$ISTAT <- as.integer(data22$ISTAT)
data19$income_more_120_tot <- gsub(";;", "", data19$income_more_120_tot)
data19$income_more_120_tot <- as.numeric(data19$income_more_120_tot)
data20$income_more_120_tot <- gsub(";;", "", data20$income_more_120_tot)
data20$income_more_120_tot <- as.numeric(data20$income_more_120_tot)
data21$income_more_120_tot <- gsub(";;", "", data21$income_more_120_tot)
data21$income_more_120_tot <- as.numeric(data21$income_more_120_tot)
data22$income_more_120_tot <- gsub(";", "", data22$income_more_120_tot)
data22$income_more_120_tot <- as.numeric(data22$income_more_120_tot)



#union of the datasets
data_inequality <- bind_rows(data00, data01, data02, data03, data04, data05, data06, data07, data08, data09, data10, data11, data12, data13)

#dataset with income data between 2014 and 2023
data_inequality2 <- bind_rows(data14, data15, data16, data17, data18, data19, data20, data21, data22, data23)

data_inequality <- bind_rows (data_inequality, data_inequality2)

#save the dataset
write.csv(data_inequality, "data/basic_data/data_inequality_italy.csv", row.names = FALSE)


#inequality calculations: using the code for generalized pareto interpolation developed by Blanchet et al.
#deal with missing values assigning 0 to all of them:
data_inequality <- data_inequality %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x) | is.nan(.x), 0)))


#create income fractiles and total working population
data_inequality <- data_inequality %>%
  rowwise() %>%
  mutate (
    tot_pop = (income_less_0_freq + income_0_10_freq + income_10_15_freq + income_15_26_freq + income_26_55_freq + income_55_75_freq + income_75_120_freq + income_more_120_freq), 
    fractile_10 = (income_less_0_freq + income_0_10_freq) / tot_pop,
    fractile_15 = (income_less_0_freq + income_0_10_freq + income_10_15_freq) / tot_pop,
    fractile_26 = (income_less_0_freq + income_0_10_freq + income_10_15_freq + income_15_26_freq) / tot_pop,
    fractile_55 = (income_less_0_freq + income_0_10_freq + income_10_15_freq + income_15_26_freq + income_26_55_freq) / tot_pop,
    fractile_75 = (income_less_0_freq + income_0_10_freq + income_10_15_freq + income_15_26_freq + income_26_55_freq + income_55_75_freq) / tot_pop,
    fractile_120 = (income_less_0_freq + income_0_10_freq + income_10_15_freq + income_15_26_freq + income_26_55_freq + income_55_75_freq + income_75_120_freq) / tot_pop
  ) %>%
  ungroup()

#average income
data_inequality <- data_inequality %>%
  rowwise() %>%
  mutate(
    tot_income = income_0_10_tot + income_10_15_tot + income_15_26_tot +
      income_26_55_tot + income_55_75_tot + income_75_120_tot + income_more_120_tot,
    average_income = tot_income / tot_pop,
    average_0_10 = income_0_10_tot / (income_0_10_freq + income_less_0_freq),
    average_10_15 = income_10_15_tot / income_10_15_freq,
    average_15_26 = income_15_26_tot / income_15_26_freq,
    average_26_55 = income_26_55_tot / income_26_55_freq,
    average_55_75 = income_55_75_tot / income_55_75_freq,
    average_75_120 = income_75_120_tot / income_75_120_freq,
    average_more_120 = income_more_120_tot / income_more_120_freq
  ) %>%
  ungroup()

#remove observations in which the first 3 income brackets are 0:
data_inequality <- data_inequality %>%
  filter(if_all(all_of(c("average_0_10", "average_10_15", "average_15_26", "average_26_55")), ~ is.finite(.)))

#gpinter for generalized pareto interpolation
data_inequality <- data_inequality %>%
  rowwise() %>%
  mutate(
    average = average_income,
    raw_a = list(c_across(c(average_10_15, average_15_26, average_26_55, average_55_75, average_75_120, average_more_120))),
    raw_p = list(c_across(c(fractile_10, fractile_15, fractile_26, fractile_55, fractile_75, fractile_120))),
    raw_q = list(c(10000, 15000, 26000, 55000, 75000, 120000)),
    valid_idx = list(which(!is.na(raw_a))),
    a = list(raw_a[valid_idx]),
    p = list(raw_p[valid_idx]),
    q = list(raw_q[valid_idx])
  ) %>%
  ungroup() %>%
  select(-raw_a, -raw_p, -raw_q, -valid_idx)

#create continuous distribution
data_inequality <- data_inequality %>%
  mutate(
    distribution = pmap(
      list(p = p, q = q, a = a, average = average),
      function(p, q, a, average) {
        tabulation_fit(p = p, threshold = q, bracketavg = a, average = average)
      }
    )
  )

#construct inequality indicators
data_inequality <- data_inequality %>%
  rowwise() %>%
  mutate(
    gini_index = gini(distribution),
    top_5 = top_share(distribution, 0.95), 
    top_01 = top_share(distribution, 0.999)
  ) %>%
  ungroup()

data_inequality <- data_inequality %>%
  rename (
    poverty = fractile_10
  )
data_inequality_clean <- data_inequality %>%
  select (
    year, ISTAT, municipality, province, region, average_income, gini_index, top_5, top_01, poverty
  )

#Save the datasets
write.csv(data_inequality_clean, "data/processed_data/inequality_data/data_inequality_complete_italy.csv", row.names = FALSE)
write.csv(data_inequality_clean, "data/processed_data/inequality_data/data_inequality_complete.csv", row.names = FALSE)

#THEIL INDEX
data_inequality <- data_inequality %>%
  filter(
    year %in% c (2000, 2011, 2019, 2023)
  )
data_inequality <- data_inequality %>%
  rowwise() %>%
  mutate(
    simulated_income = list(simulate_gpinter(distribution, n = 10000)),
    theil_index = Theil(simulated_income, parameter = 0)
  ) %>%
  ungroup()

data_theil <- data_inequality %>%
  select (
    year, ISTAT, municipality, province, region, tot_pop, tot_income, average_income, theil_index
  )
write.csv (data_theil, "data/processed_data/inequality_data/data_theil.csv")
