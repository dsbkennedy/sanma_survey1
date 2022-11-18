

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


library(broom)
library(emmeans)
devtools::install_github("wilkelab/ungeviz")
library(ungeviz)

### Compare with Tafea data
tafea_snf_data <- read_xlsx(here('/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/analysis/van_tafea_survey1/data/input/Tafea_form 11_FOR STATA.xlsx'))

tafea_snf_estimates <- tafea_snf_data  %>% 
  select(mda_code_upd=mda_code,village_final=f11_village,ascaris_epg,trichuris_epg, hookworm_epg) %>% 
  pivot_longer(-c(mda_code_upd,village_final)) %>% mutate(pos_neg=case_when(value>0 ~ 1, 
                                                                            TRUE ~ 0)) %>% 
  lm(pos_neg ~ name, data = .) %>% 
  emmeans(c("name")) %>%
  tidy() %>% mutate(location='Tafea')

sanma_snf_estimates <- long_data %>% filter(data_entry==1) %>% 
  lm(pos_neg ~ name, data = .) %>% 
  emmeans(c("name")) %>%
  tidy() %>% mutate(location='Sanma')

tafea_snf_estimates %>% bind_rows(sanma_snf_estimates) %>% mutate(name_upd=case_when(grepl('trichuris', name) ~ 'Trichuris',
                                                                                     grepl('hookworm', name) ~ 'Hookworm',
                                                                                     grepl('ascaris', name) ~ 'Ascaris')) %>% 
  mutate(name_upd=factor(name_upd, levels=c('Ascaris', 'Trichuris', 'Hookworm'))) %>% 
  ggplot(., aes(y = location, moe = std.error, x = estimate)) +
  stat_confidence_density(aes(fill = location), height = 0.8, confidence = 0.68) +
  geom_point(aes(x = estimate), size = 2) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.5) +
  #xlim(0, 1) +
  theme_bw() +
  labs(x='', y='') +
  scale_x_continuous(labels = scales::percent,limits = c(0, 0.55), 
                     breaks = c(0, 0.1,  0.2,  0.30, 0.4,  0.5)) +
  facet_wrap(~name_upd, ncol=1)

ggplot(., aes(y = name, moe = std.error, x = estimate)) +
  stat_confidence_density(fill = "red", height = 0.8, confidence = 0.68) +
  geom_point(aes(x = estimate), size = 2) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.5) +
  xlim(0, 1) +
  theme_minimal() 

prev_fn <- function(data) { 
  cacao_means <- lm(pos_neg ~ village_final, data = data) %>% 
    emmeans(c("village_final")) %>%
    tidy() %>%
    mutate(location = fct_reorder(village_final, estimate))
}


library(ggridges)

tafea_snf_estimates %>% bind_rows(sanma_snf_estimates) %>% mutate(name_upd=case_when(grepl('trichuris', name) ~ 'Trichuris',
                                                                                     grepl('hookworm', name) ~ 'Hookworm',
                                                                                     grepl('ascaris', name) ~ 'Ascaris')) %>% 
  mutate(name_upd=factor(name_upd, levels=c('Ascaris', 'Trichuris', 'Hookworm'))) %>% 
  ggplot(., aes(x = estimate, y = location)) +
  stat_confidence_density(
    aes(moe = std.error, height = stat(density),fill=location), geom = "ridgeline",
    confidence = 0.68,  alpha = 0.8, scale = 0.08,
    min_height = 0.1
  ) +
  geom_vpline(aes(x = estimate), size = 1.5, height = 0.5, color = "#D55E00") +
  theme_bw() +
  labs(x='', y='') +
  scale_x_continuous(labels = scales::percent,limits = c(0, 0.6), 
                     breaks = c(0, 0.1,  0.2,  0.30, 0.4,  0.5)) +
  facet_wrap(~name_upd, ncol=1, scales='free_y')
# https://wilkelab.org/ungeviz/articles/misc-geoms-stats.html


prev_fn <- function(data) { 
  cacao_means <- lm(pos_neg ~ name, data = data) %>% 
    emmeans(c("name")) %>%
    tidy() 
}

tafea_long_data <- read_xlsx(here('/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/analysis/van_tafea_survey1/data/input/Tafea_form 11_FOR STATA.xlsx')) %>%   select(mda_code_upd=mda_code,village_final=f11_village,ascaris_epg,trichuris_epg, hookworm_epg) %>% 
  pivot_longer(-c(mda_code_upd,village_final)) %>% mutate(pos_neg=case_when(value>0 ~ 1, 
                                                                            TRUE ~ 0)) %>% mutate(location='Tafea')

sanma_tafea_snf_data <- long_data %>% filter(data_entry==1) %>% 
  mutate(location='Sanma') %>% 
  bind_rows(tafea_long_data)

snf_estimates <- sanma_tafea_snf_data %>% 
  group_nest(location,village_final) %>% 
  mutate(model=map(data,prev_fn)) %>% 
  unnest(model) %>% mutate(name_upd=case_when(grepl('trichuris', name) ~ 'Trichuris',
                                              grepl('hookworm', name) ~ 'Hookworm',
                                              grepl('ascaris', name) ~ 'Ascaris')) %>% 
  mutate(name_upd=factor(name_upd, levels=c('Ascaris', 'Trichuris', 'Hookworm'))) 

snf_estimates %>% ggplot(aes(x=location, y=estimate)) +
  #geom_beeswarm() +
  geom_point(position = position_jitter(width = 0.3, height = 0), size = 1.5) +
  geom_hpline(aes(colour = location), stat = "summary", width = 0.6, size = 1.5) +
  scale_color_brewer(type = "qual", palette = 2, guide = "none") +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x='', y='') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ name_upd)
