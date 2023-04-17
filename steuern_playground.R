library(tidyverse)
library(readxl)


jur <- read_excel("direktebundessteuernjuristischepersonen.xlsx", sheet = 5,
                   skip = 7)

col_names_jur <- c("knt_nr", "knt_name", "gde_nr", "gde_name", "bezirk_nr", "bezirk_name",
                   "kreis_nr", "kreis_name", "agglo_nr", "agglo_name", "ms_region_nr",
                   "ms_region_name", "arbeitsmarktregion_nr", "arbeitsmarktregion_name",
                   "einwohner", "jur_steuerpflichtige", "jur_steuerertrag", "jur_kopfquote")


colnames(jur) <- col_names_jur

dta_jur <- jur %>% 
  select(gde_nr, gde_name, knt_name, einwohner, jur_steuerpflichtige, jur_steuerertrag, jur_kopfquote) %>% 
  mutate(gde_nr = as.numeric(gde_nr))

df_nat_steuerertrag <- read_excel("direktebundessteuernnatürlichepersonen.xlsx", sheet = 36,
                                  skip = 9)


col_names_df_nat_steuerertrag <- c("knt_nr", "gde_nr", "gde_name", "nummer", 
                                   "kreis_nr", "nat_steuerertrag", "nat_kopfquote", "einwohner")


colnames(df_nat_steuerertrag) <- col_names_df_nat_steuerertrag

df_nat_pflichtige <- read_excel("direktebundessteuernnatürlichepersonen.xlsx", sheet = 14,
                                skip = 11) %>%
  select(`Gemeinde-\r\nnummer`, Total) %>% 
  rename(gde_nr = `Gemeinde-\r\nnummer`,
         nat_steuerpflichtige = Total)


df_nat_gesamt <- left_join(df_nat_steuerertrag, df_nat_pflichtige, by= "gde_nr") %>% 
  mutate(gde_nr = as.numeric(gde_nr)) %>% 
  select(-c(gde_name, einwohner))


df_gesamt <- left_join(dta_jur, df_nat_gesamt, by = "gde_nr") %>% 
  filter(!is.na(gde_nr))

write_csv(df_gesamt, "df_nat_und_jur_gesamt.csv")

