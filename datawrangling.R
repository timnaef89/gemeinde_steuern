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

nachbarn <- read_rds("nachbarn.RDS")


# join nachbarn zu df_gesamt

df_gesamt2 <- left_join(df_gesamt, nachbarn, by = c("gde_nr" = "BFS_NUMMER"))



# save whole dataframe as startingpoint
#write_csv(df_gesamt2, "df_gesamt_inkl_nachbarn.csv")

# read in startingpoint

#   -->df with all variables needed inkluding nachbarn und anzahl nachbarn

#dta <- read_csv("df_gesamt_inkl_nachbarn.csv")
dta <- df_gesamt2

# select all cols needed and round all ertrags-variablen


dta2 <- dta %>% 
  select(gde_nr, gde_name, knt_name, einwohner, jur_steuerpflichtige:jur_kopfquote,
         nat_steuerertrag:anzahl_nachbarn) %>% 
  mutate(jur_steuerertrag = round(as.numeric(jur_steuerertrag)),
         jur_kopfquote = round(as.numeric(jur_kopfquote)),
         nat_steuerertrag = round(as.numeric(nat_steuerertrag)),
         nat_kopfquote = round(as.numeric(nat_kopfquote)))

# create the variables needed

# •	jur_prozent
# •	gde_name
# •	steuerertrag_gesamt
# •	jur_prozent
# •	jur_steuerertrag
# •	nat_prozent
# •	nat_steuerertrag
# •	nat_steuerpflichtige
# •	nat_nicht_pflichtige
# •	jur_rang
# •	gde_anzahl_gesamt
# •	anzahl_nachbarn
# •	nachbarn
# •	ranking_nachbarn
# •	erster_nachbar
# •	erster_nachbar_prozent
# •	letzter_nachbar
# •	letzter_nachbar_prozent
# •	jur_steuerpflichtige
# •	jur_kopfquote
# •	nat_kopfquote

dta3 <- dta2 %>% 
  mutate(steuerertrag_gesamt = nat_steuerertrag + jur_steuerertrag,
         jur_prozent = round(100 / steuerertrag_gesamt * jur_steuerertrag, 1),
         nat_prozent = round(100 / steuerertrag_gesamt * nat_steuerertrag, 1)) 
  
  
  
  dta4 <- dta3 %>% 
    select(gde_nr,gde_name, nachbarn) %>% 
    unnest(nachbarn) %>% 
    left_join(dta3 %>% select(gde_nr, jur_prozent), by = c("nachbarn" = "gde_nr")) %>% 
    arrange(gde_nr, desc(jur_prozent))
  
  
  index <- 1
  
  bfs_nr <- ""
  
  
  for (i in seq_len(nrow(dta4))) {
  
    if(bfs_nr != dta4$gde_nr[i]){
      
      index <- 1
      
      bfs_nr <- dta4$gde_nr[i]
    }
    
    dta4[i, "var_name"] <- paste0("nachbar", index)
    
      index <- index + 1
  }
    
  
  df_nr_name <- dta4 %>% select(gde_nr, gde_name) %>% unique()
  
  nr_name <- df_nr_name$gde_name
  
  names(nr_name) <- df_nr_name$gde_nr
  
  dta5 <- dta4 %>% 
    mutate(nachbarn = nr_name[as.character(nachbarn)]) %>% 
    group_by(gde_nr) %>% 
    mutate(rang = min_rank(desc(jur_prozent))) %>% 
    ungroup()
  
  # save for second json and graphic
  
  write_rds(dta5, "rangliste_longformat.rds")
  
  
  # take out first, gde and last
  
  dta6 <- dta5 %>% 
    group_by(gde_nr) %>% 
    mutate(var_name2 = ifelse(rang == 1, "first",
                              ifelse(rang == max(rang), "last",""))) %>% 
    filter(rang == 1 | rang == max(rang) | gde_name == nachbarn) %>% 
    ungroup() %>% 
    select(-c("var_name"))
  
  dta_tmp1 <- dta6 %>% 
    group_by(gde_nr) %>% 
    mutate(rang = ifelse(rang == max(rang), "letzter", 
                         ifelse(rang == 1, "erster", rang))) %>% 
    select(gde_nr, nachbarn, jur_prozent, rang) %>% 
    mutate(rang2 = rang) %>% 
    filter(rang == "erster" | rang == "letzter") %>% 
    ungroup() %>% 
     pivot_wider(gde_nr,
                names_from = rang,
                values_from = c(nachbarn, jur_prozent),
                values_fn = list) %>% 
    unnest(cols = everything())
  
  
  
  dta_tmp2 <- dta6 %>% 
    filter(var_name2 != "first" & var_name2 != "last") %>% 
    select(gde_nr, rang)
  
  dta_tmp3 <- left_join(dta_tmp1, dta_tmp2)
  
  
  
  # join with dta3 for dta_arria1
  
  
  dta_arria1 <- left_join(dta3, dta_tmp3) %>% 
    select(-nachbarn) %>% 
    #jur_rang_chweit
    mutate(jur_rang_ch = min_rank(desc(jur_prozent)),
           gde_anzahl_ch = nrow(dta3)-1) %>% 
    group_by(knt_name) %>% 
    mutate(jur_rang_knt = min_rank(desc(jur_prozent))) %>% 
    ungroup() %>% 
    mutate(steuerertrag_gesamt_mio = steuerertrag_gesamt / 1000000,
           jur_steuerertrag_mio = jur_steuerertrag / 1000000,
           nat_steuerertrag_mio = nat_steuerertrag / 1000000,
           verortung_ch = 10 - ntile(jur_prozent, 10) +1)
  
  

  
  # create dta_arria2
  
  dta_arria2 <- dta5 %>% 
    select(-var_name)
  
  # rang noch übergeben
  
  
  rang_arria1 <- dta_arria2 %>% 
    filter(gde_name == nachbarn) %>% 
    select(gde_nr, rang) %>% 
    rename(rang_nachbarschaft = rang)
  
  dta_arria1 <- left_join(dta_arria1, rang_arria1) %>% 
    mutate(verortung_nachbarschaft = ifelse(anzahl_nachbarn/2 >= rang_nachbarschaft, "top", "bottom"))
  
  # save 
  
  write_csv(dta_arria1, "dta_arria1.csv")
  
  write_csv(dta_arria2, "dta_arria2.csv")
  
  # create json for arria
  
  source("get_gde_required.R")
# all gde
  
  gde_required <- bfs_gde_for_gde_seiten %>% 
    select(Gemeinde_Nr)
  
  # first a test json
  
  test_json <- list(all_gde = dta_arria1 %>% 
                      filter(gde_nr %in% gde_required$Gemeinde_Nr) %>% 
                      sample_n(15) %>% 
    mutate(s = map(gde_nr, function(nr) dta_arria2 %>% 
                     filter(gde_nr == nr))))
  
  jsonlite::write_json(test_json, "test_json.json")
  
  
  # für gesamte daten folgenden code auskommentieren und auführen:
  
  # dta_json <- list(all_gde = gde_required %>% 
  #                     mutate(t = map(Gemeinde_Nr, function(nr) dta_arria1 %>% 
  #                                      filter(gde_nr == nr)),
  #                            s = map(Gemeinde_Nr, function(nr) dta_arria2 %>% 
  #                                      filter(gde_nr == nr))))
  
  
