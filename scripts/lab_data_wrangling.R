pacman::p_load(tidyverse, here, janitor, readxl, linelist)

col_names <- c('line', 'mda_code', 'pellet_vol', 
               'ascaris_egg_1', 'ascaris_egg_2',
               'trichuris_egg_1', 'trichuris_egg_2',
               'hookworm_egg_1', 'hookworm_egg_2', 
               'other_egg_1', 'other_egg_2', 
               'ascaris_epg', 'trichuris_epg', 'hookworm_epg', 'other_epg')

survey_villages <- read_csv(here('data', 'input', 'sanma_survey_villages.csv')) %>% 
  clean_data()

# First data entry --------------------------------------------------------

first_data_entry_folder_path <- '/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/Vanuatu NTD ME/Data/Lab results/Sanma/daily_results/'
first_data_entry_files_path <- list.files(first_data_entry_folder_path, pattern='.xlsx', full.names=T)

read_fn1 <- function(x) {
  lab_tech <- read_xlsx(x) %>% select(6) %>% 
    filter_at(1, all_vars(grepl('microscopist', ., ignore.case = T))) %>%
    rename('lab_tech'=1) %>% 
    separate(lab_tech, ':', into=c('text', 'lab_tech')) %>% select(lab_tech) 
    #mutate(label='lab_tech')
  
  date <- read_xlsx(x) %>%
    select(1) %>%
    filter_at(1, all_vars(grepl('date', ., ignore.case = T))) %>%
    rename('date'=1) %>%
    separate(date, ':', into=c('text', 'date_text')) %>%
    mutate(date_fmt=lubridate::parse_date_time(date_text, orders=c('mdy', 'dmy'))) %>% 
    select(date=date_fmt) 
    #mutate(label='date')
  
  village <- read_xlsx(x) %>% select(1) %>% 
    filter_at(1, all_vars(grepl('village', ., ignore.case = T))) %>%
    rename('village'=1) %>%
    separate(village, ':', into=c('text', 'village')) %>% select(village) 
    # mutate(village=case_when(is.na(village) ~ 'Missing', TRUE ~ village)) %>% mutate(label='village') %>% 
    # mutate(path=x)
  
  data <- read_xlsx(x, skip=15) %>%
    mutate(across(everything(), as.character)) %>%
    rename_at(vars(names(.)), ~col_names) %>%
    mutate(lab_tech=lab_tech$lab_tech) %>%
    mutate(date=date$date) %>%
    mutate(village=village$village)

  #add_vars <- data %>% bind_cols(date, lab_tech, village)
  
  #merged_data <- data %>% bind_cols(add_vars)
  #merged_data <- data %>% bind_cols(date,village)
  #merged_data <- data %>% bind_cols(date, village, lab_tech)
  
  
  return(data)
}
library(furrr)
plan(multicore)
first_data_entry <- map_dfr(first_data_entry_files_path, read_fn1) %>% mutate(data_entry=1) 

first_data_entry_clean <- first_data_entry %>% 
  clean_data() %>% 
  filter(!is.na(mda_code) & !is.na(pellet_vol)) %>% 
  mutate(village_upd=case_when((village=='sarakata_mataloi' & lab_tech=='aaron_nako' & line %in% c(1:10) ~ 'sarakata'),
                               (village=='sarakata_mataloi' & lab_tech=='aaron_nako' & line %in% c(11:20) ~ 'mataloi'),
                               (village=='mataloi_narango' & lab_tech=='aaron_nako' & line %in% c(1:12) ~ 'mataloi'),
                               (village=='mataloi_narango' & lab_tech=='aaron_nako' & line %in% c(13:20) ~ 'narango'),
                               (village=='wunpuku' & lab_tech=='aaron_nako' & line %in% c(1:20) ~ 'wunpuka'),
                               (village=='wunpuku_fanafo' & lab_tech=='aaron_nako' & line %in% c(1:8) ~ 'wunpuka'),
                               (village=='wunpuku_fanafo' & lab_tech=='aaron_nako' & line %in% c(9:20) ~ 'fanafo'),
                               (village=='pwelvus' & lab_tech=='aaron_nako' & line %in% c(1:20) ~ 'pwelwus'),
                               (village=='naveli_wusi' & lab_tech=='aaron_nako' & line %in% c(1:18) ~ 'navele'),
                               (village=='naveli_wusi' & lab_tech=='aaron_nako' & line %in% c(19:20) ~ 'wusi'),
                               (village=='pwelvus_hokwa' & lab_tech=='billy_waka' & line %in% c(1:14) ~ 'pwelwus'),
                               (village=='pwelvus_hokwa' & lab_tech=='billy_waka' & line %in% c(15:20) ~ 'hokua'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(1:8) ~ 'asaviah'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(9:9) ~ 'tanavoli'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(10:11) ~ 'talua'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(1:8) ~ 'asaviah'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(1:8) ~ 'asaviah'),
                           TRUE ~ village))

first_village_check <- first_data_entry_clean %>% 
  #select(village) %>%  
  clean_data() %>% 
  anti_join(survey_villages, by=c('village_upd' = 'village')) %>% 
  select(line,date,mda_code,lab_tech,contains('village'))


# Second data entry -------------------------------------------------------

second_data_entry_folder_path <- '/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/Vanuatu NTD ME/Data/Lab results/Sanma/double_entry_data'
second_data_entry_files_path <- list.files(second_data_entry_folder_path, pattern='.xlsx', full.names=T)
  
  aaron_path <- Filter(function(x) grepl('aaron', x, ignore.case=T), second_data_entry_files_path)
  aaron_sheets <- excel_sheets(aaron_path)
  
  billy_path <- Filter(function(x) grepl('billy', x, ignore.case=T), second_data_entry_files_path)
  billy_sheets <- excel_sheets(billy_path[1])
  
  maryann_path <- Filter(function(x) grepl('mary', x, ignore.case=T), second_data_entry_files_path)
  maryann_sheets <- excel_sheets(maryann_path)
  
  rina_path <- Filter(function(x) grepl('rina', x, ignore.case=T), second_data_entry_files_path)
  rina_sheets <- excel_sheets(rina_path)
  
  
  read_fn <- function(x,y) {
    data <- read_xlsx(x, sheet=y, skip=15) %>% 
      mutate(across(everything(), as.character)) %>% 
      mutate(village=y) %>% 
      rename_at(vars(names(.)), ~col_names)
    
    date <- read_xlsx(x, sheet=y) %>% 
      select(1) %>% 
      filter_at(1, all_vars(grepl('date', ., ignore.case = T))) %>% 
      rename('date'=1) %>% 
      separate(date, ':', into=c('text', 'date_text')) %>% 
      mutate(date_fmt=lubridate::parse_date_time(date_text, orders=c('mdy', 'dmy'))) %>% select(date=date_fmt)
    
    merged_data <- data %>% bind_cols(date)
    
    return(merged_data)
  }

aaron_data <- map2_dfr(aaron_path,aaron_sheets, read_fn) %>% mutate(lab_tech='aaron')

billy_data <- map2_dfr(billy_path,billy_sheets, read_fn) %>% mutate(lab_tech='billy')

maryann_data <- map2_dfr(maryann_path,maryann_sheets, read_fn) %>% mutate(lab_tech='maryann')

rina_data <- map2_dfr(rina_path,rina_sheets, read_fn) %>% mutate(lab_tech='rina')

second_data_entry <- aaron_data %>% bind_rows(billy_data, maryann_data, rina_data)  %>% mutate(data_entry=2)
  
rm(list=setdiff(ls(), c("first_data_entry", "second_data_entry")))

all_data_entry <- first_data_entry %>% bind_rows(second_data_entry) %>% mutate(village=case_when(is.na(village) ~ location,
                                                                                                 TRUE ~ village)) %>% 
  filter(!is.na(mda_code) & !is.na(pellet_vol)) %>% 
  mutate(date_clean=case_when(date==ymd(20220111) ~ ymd(20221101), 
                              date==ymd(20220211) ~ ymd(20221102), 
                              date==ymd(20220311) ~ ymd(20221103), 
                              date==ymd(20220411) ~ ymd(20221104), 
                              date==ymd(20221128) ~ ymd(20221028), 
                              TRUE ~ ymd(date))) %>% 
  mutate(late_pd_samples=case_when(grepl("L", mda_code) ~1, TRUE ~ 0)) %>% 
  mutate(mda_code_upd=str_replace(mda_code, "\\s[^ ]+$", "")) %>% 
  mutate(mda_code_upd=str_replace(mda_code_upd, 'L', '')) %>% 
  mutate(across(.cols = ascaris_egg_1:other_epg, str_extract, pattern = "\\d+", .names="{.col}_upd")) %>% 
  mutate(across(.cols = ascaris_egg_1_upd:other_epg_upd, as.numeric)) %>% 
  mutate(lab_tech_upd=case_when(grepl('aaron', lab_tech, ignore.case=T) ~ 'aaron', 
                                grepl('mary', lab_tech, ignore.case=T) ~ 'maryann',
                                grepl('rina', lab_tech, ignore.case=T) ~ 'rina',
                                grepl('billy', lab_tech, ignore.case=T) ~ 'billy'))


dup_check <- all_data_entry %>% clean_data() %>%  count(mda_code_upd, village, date_clean)


