# Wrangling stool inventory (form10
library(tidyverse)
library(here)
library(janitor)

filepath <-
  here('data', 'input', 'INVENTORY - SANTO SAMPLE CODES.xlsx')
list_sheets <- readxl::excel_sheets(filepath)

all_data <-
  purrr::map_df(list_sheets,
                ~ dplyr::mutate(readxl::read_excel(
                  filepath, sheet = .x, skip = 6
                ),
                location = .x)) %>% 
  clean_names() %>% 
  select(-x2) %>%  
  filter(!grepl('TOTAL', mda_code, ignore.case = T)) %>% 
  mutate(PD_in_lab=case_when(grepl('L', mda_code) ~ 'Yes'))

all_data %>% count(PD_in_lab)

write_csv(all_data, 'data/output/vanuatu_sanma_stool_inventory_20221031.csv')
