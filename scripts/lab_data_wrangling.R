pacman::p_load(tidyverse, here, janitor, readxl, linelist, lubridate, ggbeeswarm)

col_names <- c('line', 'mda_code', 'pellet_vol', 
               'ascaris_egg_1', 'ascaris_egg_2',
               'trichuris_egg_1', 'trichuris_egg_2',
               'hookworm_egg_1', 'hookworm_egg_2', 
               'other_egg_1', 'other_egg_2', 
               'ascaris_epg', 'trichuris_epg', 'hookworm_epg', 'other_epg')

survey_villages <- read_csv(here('data', 'input', 'sanma_survey_villages.csv')) %>% 
  clean_data() %>% mutate(survey_village=1)

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

first_data_entry <- map_df(first_data_entry_files_path, read_fn1) %>% mutate(data_entry=1) 

first_data_entry_clean <- first_data_entry %>% 
  clean_data() %>% 
  filter(!is.na(mda_code) & !is.na(pellet_vol)) %>% 
  mutate_all(na_if,"") %>% 
  mutate(village_upd=case_when((village=='sarakata_mataloi' & lab_tech=='aaron_nako' & line %in% c(1:10) ~ 'sarakata_darkona'),
                               (village=='sarakata_mataloi' & lab_tech=='aaron_nako' & line %in% c(11:20) ~ 'mataloi'),
                               (village=='mataloi_narango' & lab_tech=='aaron_nako' & line %in% c(1:12) ~ 'mataloi'),
                               (village=='mataloi_narango' & lab_tech=='aaron_nako' & line %in% c(13:20) ~ 'narango'),
                               (village=='wunpuku' & lab_tech=='aaron_nako' & line %in% c(1:20) ~ 'wunpuka'),
                               (village=='wunpuku_fanafo' & lab_tech=='aaron_nako' & line %in% c(1:8) ~ 'wunpuka'),
                               (village=='wunpuku_fanafo' & lab_tech=='aaron_nako' & line %in% c(9:20) ~ 'fanafo'),
                               (village=='pwelvus' & lab_tech=='aaron_nako' & line %in% c(1:20) ~ 'pwelwus'),
                               (village=='naveli_wusi' & lab_tech=='aaron_nako' & line %in% c(1:18) ~ 'navele'),
                               (village=='naveli_wusi' & lab_tech=='aaron_nako' & line %in% c(19:20) ~ 'wusi'),
                               (village=='assevaia' & lab_tech=='aaron_nako' & line %in% c(1:20) ~ 'asafia'),
                               (village=='pwelvus_hokwa' & lab_tech=='billy_waka' & line %in% c(1:14) ~ 'pwelwus'),
                               (village=='pwelvus_hokwa' & lab_tech=='billy_waka' & line %in% c(15:20) ~ 'hokua'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(1:8) ~ 'asafia'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(9:9) ~ 'tanavoli'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(10:11) ~ 'talua'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(12:16) ~ 'tangoa'),
                               (village=='asevaiah_tanavoli_talua_tangoa_and_molboe' & lab_tech=='rina_morrison' & line %in% c(17:20) ~ 'molboe'),
                               (village=='tangoa_bethany' & lab_tech=='rina_morrison' & line %in% c(1:5) ~ 'tangoa'),
                               (village=='tangoa_bethany' & lab_tech=='rina_morrison' & line %in% c(6:20) ~ 'pentani'),
                               (village=='nawelala_and_kerepua' & lab_tech=='rina_morrison' & line %in% c(1:6) ~ 'nawelala'),
                               (village=='nawelala_and_kerepua' & lab_tech=='rina_morrison' & line %in% c(7:20) ~ 'kerepua'),
                               (village=='nawelala_and_wusi' & lab_tech=='rina_morrison' & line %in% c(1:12) ~ 'nawelala'),
                               (village=='nawelala_and_wusi' & lab_tech=='rina_morrison' & line %in% c(13:15) ~ 'wusi'),
                               (village=='nawelala_and_wusi' & lab_tech=='rina_morrison' & line %in% c(16:19) ~ 'malo'),
                               (is.na(village) & lab_tech=='rina_morrison' ~ 'malo'),
                               (village=='barrick_and_banban' & lab_tech=='rina_morrison' & line %in% c(1:15) ~ 'barrick'),
                               (village=='barrick_and_banban' & lab_tech=='rina_morrison' & line %in% c(16:20) ~ 'banban_park_labetra'),
                               (village=='malatoi' & lab_tech=='rina_morrison' & line %in% c(1:20) ~ 'mataloi'),
                               (village=='unknown' & lab_tech=='rina_morrison' & line %in% c(1:20) ~ 'malo'),
                               (village=='sda_mission_valvalet' & lab_tech=='rina_morrison' & line %in% c(1:11) ~ 'sda_mission'),
                               (village=='sda_mission_valvalet' & lab_tech=='rina_morrison' & line %in% c(12:20) ~ 'valavalet'),
                               (village=='valbay' & lab_tech=='rina_morrison' & line %in% c(1:20) ~ 'valpei'),
                               (village=='valbay_and_malo' & lab_tech=='rina_morrison' & line %in% c(1:10) ~ 'valpei'),
                               (village=='valbay_and_malo' & lab_tech=='rina_morrison' & line %in% c(11:20) ~ 'malo'),
                               (village %in% c('assevaia','asevaiah') & lab_tech=='rina_morrison' & line %in% c(1:20) ~ 'asafia'),
                               (village=='valvalet_molboe' & lab_tech=='rina_morrison_billy_nauka' & line %in% c(1:17) ~ 'valavalet'),
                               (village=='valvalet_molboe' & lab_tech=='rina_morrison_billy_nauka' & line %in% c(18:20) ~ 'molboe'),
                               (village=='solway_banban_malatoi' & lab_tech=='mary_ann_tosul' & line %in% c(1:4) ~ 'solway_2'),
                               (village=='solway_banban_malatoi' & lab_tech=='mary_ann_tosul' & line %in% c(5:5) ~ 'banban_park_labetra'),
                               (village=='solway_banban_malatoi' & lab_tech=='mary_ann_tosul' & line %in% c(6:12) ~ 'mataloi'),
                               (village=='tanavoli_village' & lab_tech=='mary_ann_tosul' ~ 'tanavoli'),
                               (village=='talua_village' & lab_tech=='mary_ann_tosul' ~ 'talua'),
                               (village=='narango_village_south_santo' & lab_tech=='mary_ann_tosul' ~ 'narango'),
                               (village=='not_indicated' ~ 'malo'),
                           TRUE ~ village))

first_village_check <- first_data_entry_clean %>% 
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
      rename_at(vars(names(.)), ~col_names) %>% 
      mutate(village=y) 
    
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

second_data_entry_clean <- second_data_entry %>% 
  clean_data() %>% 
  filter(!is.na(mda_code) & !is.na(pellet_vol)) %>% 
  mutate_all(na_if,"") %>% 
  mutate(village_upd=case_when((village=='sarakata_mataloi' & lab_tech=='aaron' & line %in% c(1:10) ~ 'sarakata_darkona'),
                               (village=='sarakata_mataloi' & lab_tech=='aaron' & line %in% c(11:20) ~ 'mataloi'),
                               (village=='narango_mataloi' & lab_tech=='aaron' & line %in% c(1:12) ~ 'mataloi'),
                               (village=='narango_mataloi' & lab_tech=='aaron' & line %in% c(13:20) ~ 'narango'),
                               (village=='navele_wusi' & lab_tech=='aaron' & line %in% c(1:18) ~ 'navele'),
                               (village=='navele_wusi' & lab_tech=='aaron' & line %in% c(19:20) ~ 'wusi'),
                               (village %in% c('wunpuku_1', 'wunpuku_2') & lab_tech=='aaron' ~ 'wunpuka'),
                               (village=='pwelvus' & lab_tech=='aaron' ~ 'pwelwus'),
                               (village=='assevaia' & lab_tech=='aaron' ~ 'asafia'),
                               (village %in% c('nawelala_2', 'nawelala_3') & lab_tech=='aaron' ~ 'nawelala'),
                               (village %in% c('narango_2') & lab_tech=='maryann' ~ 'narango'),
                               (village %in% c('malo_1', 'malo_2', 'malo_3', 'malo_4', 'malo_5') & lab_tech=='rina' ~ 'malo'),
                               (village=='valbay' & lab_tech=='rina' & line %in% c(1:20) ~ 'valpei'),
                               (village=='asevaiah' & lab_tech=='rina' & line %in% c(1:20) ~ 'asafia'),
                               TRUE ~ village))

second_village_check <- second_data_entry_clean %>% 
  clean_data() %>% 
  anti_join(survey_villages, by=c('village_upd' = 'village')) %>% 
  select(line,date,mda_code,lab_tech,contains('village'))
  
#rm(list=setdiff(ls(), c("first_data_entry", "second_data_entry")))

all_data_entry <- first_data_entry_clean %>% bind_rows(second_data_entry_clean) %>% 
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
                                grepl('billy', lab_tech, ignore.case=T) ~ 'billy')) %>% 
  clean_data()


dup_check <- all_data_entry %>%  count(mda_code_upd, village_upd, date_clean,lab_tech_upd) %>% filter(n!=2) %>% 
  left_join(survey_villages, by=c('village_upd' = 'village')) %>% 
  select(mda_code_upd, village_upd, date_clean,lab_tech_upd,survey_village) %>% 
  mutate(village_match=case_when(survey_village==1 ~ village_upd)) %>% 
  group_by(mda_code_upd) %>% tidyr::fill(village_match, .direction = "downup") %>% 
  select(mda_code_upd,date_clean,lab_tech_upd,village_match)


final_data <- all_data_entry %>% 
  left_join(dup_check, by=c('mda_code_upd','date_clean','lab_tech_upd')) %>% 
  mutate(village_final=coalesce(village_match, village_upd)) %>% 
  full_join(survey_villages, by=c('village_final' = 'village')) 

long_data <- final_data  %>% filter(!is.na(village_final)) %>% 
  select(mda_code_upd,village_final,data_entry,ascaris_epg_upd,trichuris_epg_upd, hookworm_epg_upd) %>% 
  pivot_longer(-c(mda_code_upd,village_final,data_entry)) %>% mutate(pos_neg=case_when(value>0 ~ 1, 
                                                                                       TRUE ~ 0))


prevalence_data <- long_data %>% count(data_entry,village_final, name, pos_neg) %>% 
  group_by(data_entry) %>% 
  complete(village_final, name,pos_neg, fill=list(n=0)) 

(overall_prevalence_estimates <- prevalence_data %>%   
  group_by(data_entry,name) %>% 
    mutate(tests=sum(n)) %>% 
    filter(pos_neg==1) %>% 
    summarise(pos=sum(n,na.rm=TRUE),
              tests=max(tests,na.rm=TRUE)) %>% 
    mutate(prop = map2(pos, tests, ~ prop.test(.x, .y, conf.level=0.95) %>%
                         broom::tidy())) %>%
    unnest(prop) %>% select(data_entry, name, pos,tests,estimate,conf.low, conf.high) %>% 
    mutate(village='overall')
)

  prevalence_estimates <- prevalence_data %>%   
    group_by(data_entry,village_final, name) %>% 
  mutate(tests=sum(n)) %>% 
  arrange(data_entry,village_final, name) %>% 
  mutate(prop = map2(n, tests, ~ prop.test(.x, .y, conf.level=0.95) %>%
                           broom::tidy())) %>%
  unnest(prop) %>% select(data_entry,village_final, name, pos_neg,n,tests,estimate,conf.low, conf.high) %>% 
  group_by(data_entry,village_final, name) %>% 
  filter(pos_neg==1) %>% 
    bind_rows(overall_prevalence_estimates) %>% 
  arrange(village_final,name,data_entry)

prevalence_estimates %>% filter(data_entry==1) %>% 
  ggplot(aes(x=name, y=estimate)) + 
  geom_beeswarm() +
  stat_summary( fun=median, geom="point", shape=4, size=8, color='red') +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() 
  
  
  


