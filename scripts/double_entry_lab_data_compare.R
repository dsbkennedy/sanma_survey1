pacman::p_load(tidyverse, here, janitor, readxl, linelist)

load_all_data_fn <- function() {
folder_path <- '/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/Vanuatu NTD ME/Data/Lab results/Sanma/double_entry_data'
files_path <- list.files(folder_path, pattern='.xlsx', full.names=T)

aaron_path <- Filter(function(x) grepl('aaron', x, ignore.case=T), files_path)
aaron_sheets <- excel_sheets(aaron_path)

billy_path <- Filter(function(x) grepl('billy', x, ignore.case=T), files_path)
billy_sheets <- excel_sheets(billy_path)

maryann_path <- Filter(function(x) grepl('mary', x, ignore.case=T), files_path)
maryann_sheets <- excel_sheets(maryann_path)

rina_path <- Filter(function(x) grepl('rina', x, ignore.case=T), files_path)
rina_sheets <- excel_sheets(rina_path)

col_names <- c('line', 'mda_code', 'pellet_vol', 
          'ascaris_egg_1', 'ascaris_egg_2',
          'trichuris_egg_1', 'trichuris_egg_2',
          'hookworm_egg_1', 'hookworm_egg_2', 
          'other_egg_1', 'other_egg_2', 
          'ascaris_epg', 'trichuris_epg', 'hookworm_epg', 'other_epg', 'location')

read_fn <- function(x,y) {
  data <- read_xlsx(x, sheet=y, skip=15) %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(location=y) %>% 
    rename_at(vars(names(.)), ~col_names)
}

aaron_data <- map2_dfr(aaron_path,aaron_sheets, read_fn) %>% mutate(lab_tech='aaron')

billy_data <- map2_dfr(billy_path,billy_sheets, read_fn) %>% mutate(lab_tech='billy')

maryann_data <- map2_dfr(maryann_path,maryann_sheets, read_fn) %>% mutate(lab_tech='maryann')

rina_data <- map2_dfr(rina_path,rina_sheets, read_fn) %>% mutate(lab_tech='rina')

all_data <- aaron_data %>% bind_rows(billy_data, maryann_data, rina_data)

test <- all_data %>% str_replace_all(mda_code, "\\(.+?\\)\\", "")


pacman::p_load(qdap)
bracketX(all_data$mda_code) -> all_data$mda_code

write.csv(all_data, 
          '/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/Vanuatu NTD ME/Data/Lab results/Sanma/second_data_entry.csv')
}

second_lab_data <- load_all_data_fn()

first_lab_data <- '/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/Vanuatu NTD ME/Data/Lab results/Sanma/sanma_snf_results_20221109.xlsx'
first_lab_data <- read_xlsx(first_lab_data, sheet='data') %>% select(2:16)

col_names2 <- c('location', 'mda_code', 'pellet_vol', 
               'ascaris_egg_1', 'ascaris_egg_2',
               'trichuris_egg_1', 'trichuris_egg_2',
               'hookworm_egg_1', 'hookworm_egg_2', 
               'other_egg_1', 'other_egg_2', 
               'ascaris_epg', 'trichuris_epg', 'hookworm_epg', 'other_epg')

names(first_lab_data) <- col_names2

library(arsenal)
compare <- summary(comparedf(first_lab_data, second_lab_data, 
                             by=c('mda_code')))

compare_differences_list <- compare$diffs.table
compare_differences_summary <- compare$comparison.summary.table


# Compare prevalence by two data sources

second_lab_data %>% tabyl(location)


second_lab_data_rename <- second_lab_data %>% rename_with(~paste0(., "_second"), pellet_vol:location)

merged <- first_lab_data %>% full_join(second_lab_data_rename, by='mda_code') %>% 
  mutate(merge_status = case_when(!is.na(location) & !is.na(location_second) ~ 'Both data entry', 
                                  !is.na(location) ~ 'First data entry', 
                                  !is.na(location_second) ~ 'Second data entry'))

location_summary <- merged %>% count(location, location_second)

