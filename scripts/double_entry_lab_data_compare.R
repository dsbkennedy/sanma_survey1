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
  
  date <- read_xlsx(x, sheet=y) %>% 
    select(1) %>% 
    filter_at(1, all_vars(grepl('date', ., ignore.case = T))) %>% 
    rename('date'=1) %>% 
    separate(date, ':', into=c('text', 'date_text')) %>% 
    #mutate(date=as.character(date_text)) %>% select(date)
    mutate(date_fmt=lubridate::parse_date_time(date_text, orders=c('mdy', 'dmy'))) %>% select(date=date_fmt)
  
  merged_data <- data %>% bind_cols(date)
  
  return(merged_data)
}
}
aaron_data <- map2_dfr(aaron_path,aaron_sheets, read_fn) %>% mutate(lab_tech='aaron')
aaron_missing_date <- aaron_data %>% filter(is.na(date)) %>% select(location)

billy_data <- map2_dfr(billy_path,billy_sheets, read_fn) %>% mutate(lab_tech='billy')
billy_missing_date <- billy_data %>% filter(is.na(date)) %>% select(location)

maryann_data <- map2_dfr(maryann_path,maryann_sheets, read_fn) %>% mutate(lab_tech='maryann')
maryann_missing_date <- maryann_data %>% filter(is.na(date)) %>% select(location)

rina_data <- map2_dfr(rina_path,rina_sheets, read_fn) %>% mutate(lab_tech='rina')
rina_missing_date <- rina_data %>% filter(is.na(date)) %>% select(location)

library(lubridate)

my_col <- c("ascaris_egg_1", "hookworm_egg_2")

all_data <- aaron_data %>% bind_rows(billy_data, maryann_data, rina_data) %>% 
  mutate(date_clean=case_when(date==ymd(20220111) ~ ymd(20221101), 
                              date==ymd(20220211) ~ ymd(20221102), 
                              date==ymd(20220311) ~ ymd(20221103), 
                              date==ymd(20220411) ~ ymd(20221104), 
                              date==ymd(20221128) ~ ymd(20221028), 
                              TRUE ~ ymd(date))) %>% 
  mutate(late_pd_samples=case_when(grepl("L", mda_code) ~1, TRUE ~ 0)) %>% 
  mutate(mda_code_upd=str_replace(mda_code, "\\s[^ ]+$", "")) %>% 
  mutate(across(.cols = ascaris_egg_1:other_epg, str_extract, pattern = "\\d+", .names="{.col}_upd")) %>% 
  mutate(across(.cols = ascaris_egg_1_upd:other_epg_upd, as.numeric))



all_data %>% count(date_clean, lab_tech) %>% 
  ggplot(aes(x=date_clean, y=n)) +
  geom_col() +
  facet_wrap(~ lab_tech)


write.csv(all_data, 
          '/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/Vanuatu NTD ME/Data/Lab results/Sanma/second_data_entry.csv')


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

