

# towards creating a line chart for each MS region
library(tidyverse)

# install Datawrappr package
#devtools::install_github("munichrocker/DatawRappr")

# use custom Datawrapper functions (based on Datawrappr, but slightly different plus some extra functions, notably dw_update_and_republish)
source("custom_DW_functions.R")

# set DW API KEY
Sys.setenv(DW_API_KEY = "jBAKv9CWsroIlEa4TvBwhLcwjiWMQiaMrTRZL7mUF0ymfKmW1Bs2wzxKcdWQqJ3D")

# set folder number (as string!)
folder_region_increase <- "166667"

# load data


dta_dw <- read_csv("dta_arria2.csv")


# function that returns the data of one `region`
create_dta_per_region <- function(new_region_nr) {
  
  out <- dta_dw %>% 
    filter(gde_nr == new_region_nr) %>% 
    mutate(nachbarn = ifelse(gde_name == nachbarn, paste0("<b>", nachbarn, "</b>"), nachbarn))
  
  return(out)
  
}

# function that creates one chart (based on template `old_chart_id`) -and saves the iframe code in a table (`rpl_tbl_path`)
create_new_chart <- function(new_region_nr,
                             new_region,
                             old_chart_id = "Wq3i2" ,
                             new_folder = folder_region_increase, 
                             rpl_tbl_path) {
  
  new_dta <- create_dta_per_region(new_region_nr = new_region_nr)
  
  new_chart <- DatawRappr::dw_copy_chart(copy_from = old_chart_id,
                                         api_key = Sys.getenv("DW_API_KEY"))
  
  dw_edit_chart(chart_id = new_chart$id,
                title = paste0(new_dta$gde_name[1]," im Vergleich mit den direkten Nachbarn"),
                intro = paste0("Anteil der Abgaben von juristischen Personen (in Prozent) an allen entrichteten direkten Bundessteuern in der jeweiligen Gemeinde"),
                folderId = new_folder,
                api_key = Sys.getenv("DW_API_KEY"))
  
  dw_update_and_republish(dw_id = new_chart$id,
                          data_for_dw = new_dta)
  
  new_chart_meta <- DatawRappr::dw_retrieve_chart_metadata(chart_id = new_chart$id, 
                                                           api_key = Sys.getenv("DW_API_KEY"))
  
  new_tbl <- read_csv(rpl_tbl_path, col_types = cols(
    MS_Region_Nr = col_double(),
    MS_Region_Name = col_character(),
    iframe_full = col_character()
  )) %>% 
    add_row(MS_Region_Nr = new_region_nr,
            MS_Region_Name = new_region,
            iframe_full = new_chart_meta$content$metadata$publish$`embed-codes`$`embed-method-responsive`)
  
  write_csv(new_tbl, rpl_tbl_path)
  
  return(new_chart)
  
}

###
### Do IT
###

# create empty table to store iframe codes in it
embeds_chart_increase_empty <- tibble(MS_Region_Nr = numeric(0),
                                      MS_Region_Name = character(0),
                                      iframe_full = character(0))
write_csv(embeds_chart_increase_empty, "embeds_chart_increase_V2.csv")


# data frame with all required regions in (name and number)

source("get_gde_required.R")
# all gde

gde_required <- bfs_gde_for_gde_seiten %>% 
  select(name, Gemeinde_Nr)

ms_regions <- gde_required %>% 
  select(nr = Gemeinde_Nr, name = name) %>% 
  unique()


sample_gde <- ms_regions %>% 
  filter(nr %in% c(sample(ms_regions$nr, 3)))

walk2(sample_gde$nr, sample_gde$name,
      function(x, y) create_new_chart(new_region = y,
                                      new_region_nr = x,
                                      rpl_tbl_path = "embeds_chart_increase_V2.csv"))

# Try out for one region (Zürich)
# chart_zrh <- create_new_chart(new_region = "Zürich", new_region_nr = 1)
# 
# ee <- read_csv("embeds_chart_increase_V2.csv")
# chart_prtg <- create_new_chart(new_region = "Prättigau")