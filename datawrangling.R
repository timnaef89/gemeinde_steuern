# datawrangling

library(tidyverse)
library(readxl)



# read in data for juristische Personen pro Gemeinde

jur <- read_excel("direktebundessteuernjuristischepersonen.xlsx", sheet = 5,
                  skip = 7)

# create readable col-names
col_names_jur <- c("knt_nr", "knt_name", "gde_nr", "gde_name", "bezirk_nr", "bezirk_name",
                   "kreis_nr", "kreis_name", "agglo_nr", "agglo_name", "ms_region_nr",
                   "ms_region_name", "arbeitsmarktregion_nr", "arbeitsmarktregion_name",
                   "einwohner", "jur_steuerpflichtige", "jur_steuerertrag", "jur_kopfquote")


colnames(jur) <- col_names_jur


# select all columns needed
dta_jur <- jur %>% 
  select(gde_nr, gde_name, knt_name, einwohner, jur_steuerpflichtige, jur_steuerertrag, jur_kopfquote) %>% 
  mutate(gde_nr = as.numeric(gde_nr))



# read in data for natürliche Personen (Steuerertrag)
df_nat_steuerertrag <- read_excel("direktebundessteuernnatürlichepersonen.xlsx", sheet = 36,
                                  skip = 9)


col_names_df_nat_steuerertrag <- c("knt_nr", "gde_nr", "gde_name", "nummer", 
                                   "kreis_nr", "nat_steuerertrag", "nat_kopfquote", "einwohner")


colnames(df_nat_steuerertrag) <- col_names_df_nat_steuerertrag



# read in data for natürliche Personen (Anzahl Personen (normal und Sonderfälle))


df_nat_pflichtige <- read_excel("direktebundessteuernnatürlichepersonen.xlsx", sheet = 23,
                                skip = 11) %>% 
  slice(-1) %>% # delte französisch
  mutate_all(~ifelse(. == "*", 0, .)) # 

col_names_df_nat_pflichtige <- c("knt_nr", "gde_nr", "gde_name", "bezirk_nr", "kreis_nr", "a",
                                 "b", "c", "d", "e", "f", "g", "total")


colnames(df_nat_pflichtige) <- col_names_df_nat_pflichtige

# from character to numeric and calculate sum

df_nat_pflichtige2 <- df_nat_pflichtige %>% 
  mutate(a = as.numeric(a),
         b = as.numeric(b),
         c = as.numeric(c),
         d = as.numeric(d),
         e = as.numeric(e),
         f = as.numeric(f),
         g = as.numeric(g)) %>% 
  mutate(total = a+b+c+d+e+f+g) %>% 
  select(gde_nr, total) %>% 
  rename(nat_steuerpflichtige = total)


# read in data for steuerpflichtige ohne direkte bundessteuer


# Selbständig und unselbständig Erwerbende zusammen
df_nat_nicht_pflichtige <- read_excel("direktebundessteuernnatürlichepersonen.xlsx", sheet = 31,
                                      skip = 12) %>% 
  select(2, 6)

col_names_nicht_pflichtige1 <- c("gde_nr", "anzahl")

colnames(df_nat_nicht_pflichtige) <- col_names_nicht_pflichtige1

# Rentner und Nichterwerbstätige zusammen	
df_nat_nicht_pflichtige2 <- read_excel("direktebundessteuernnatürlichepersonen.xlsx", sheet = 32,
                                       skip = 12) %>% 
  select(2, 6)

col_names_nicht_pflichtige1 <- c("gde_nr", "anzahl2")

colnames(df_nat_nicht_pflichtige2) <- col_names_nicht_pflichtige1


df_nat_nicht_pflichtige3 <- left_join(df_nat_nicht_pflichtige, df_nat_nicht_pflichtige2) %>% 
  mutate(anzahl = as.numeric(anzahl),
         anzahl2 = as.numeric(anzahl2)) %>% 
  mutate(nat_nicht_pflichtige = anzahl + anzahl2) %>% 
  select(gde_nr, nat_nicht_pflichtige)


# join all natürliche Personen-Sachen


df_nat_gesamt <- left_join(df_nat_steuerertrag, df_nat_pflichtige2, by= "gde_nr") %>% 
  slice(-1, -2) %>% 
  mutate(gde_nr = as.numeric(gde_nr))

df_nat_gesamt2 <- left_join(df_nat_gesamt, df_nat_nicht_pflichtige3) %>% 
  select(gde_nr, nat_steuerertrag, nat_kopfquote, nat_steuerpflichtige, nat_nicht_pflichtige) 


# join jur and nat Personen

df_gesamt <- left_join(dta_jur, df_nat_gesamt2, by = "gde_nr") %>% 
  slice(-1)

write_csv(df_gesamt, "df_nat_und_jur_gesamt.csv")

# read in nachbarn (siehe helperfunctions)

nachbarn <- read_csv("nachbarsgemeinden.csv")


# join nachbarn zu df_gesamt

df_gesamt2 <- left_join(df_gesamt, nachbarn, by = c("gde_nr" = "BFS_NUMMER"))



# save whole dataframe as startingpoint
write_csv(df_gesamt2, "df_gesamt_inkl_nachbarn.csv")

# read in startingpoint

#   -->df with all variables needed inkluding nachbarn und anzahl nachbarn

dta <- read_csv("df_gesamt_inkl_nachbarn.csv")


# select all cols needed and round all ertrags-variablen


dta2 <- dta %>% 
  select(gde_nr, gde_name, knt_name, einwohner, jur_steuerpflichtige:jur_kopfquote,
         nat_steuerertrag:anzahl_nachbarn) %>% 
  mutate(jur_steuerertrag = round(as.numeric(jur_steuerertrag)),
         jur_kopfquote = round(as.numeric(jur_kopfquote)),
         nat_steuerertrag = round(as.numeric(nat_steuerertrag)),
         nat_kopfquote = round(as.numeric(nat_kopfquote)))



