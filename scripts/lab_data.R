library(tidyverse)
library(here)
library(linelist)
library(janitor)
lab_data_link <- '/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/Vanuatu NTD ME/Data/Lab results/Sanma/sanma_snf_results_20221109.xlsx'

lab_data_raw <- readxl::read_xlsx(lab_data_link, sheet='data') %>% clean_names()

dup_check <- lab_data_raw %>% count(Village, )

library(lme4)

snf_results <- lab_data_raw %>% mutate(ascaris_pos=case_when(as.numeric(ascaris_epg)>0 ~ 1, 
                                              TRUE ~ 0)) %>% 
  mutate(trichuris_pos=case_when(as.numeric(trichuris_epg)>0 ~ 1, 
                               TRUE ~ 0)) %>% 
  mutate(hookworm_pos=case_when(as.numeric(hookworm_epg)>0 ~ 1, 
                               TRUE ~ 0)) %>% 
  select(area_council,village, ascaris_pos,trichuris_pos,hookworm_pos) %>% 
  filter(!is.na(village)) %>% 
  clean_data() %>% 
  pivot_longer(-c(area_council,village))

pd = position_dodge(0.5)

snf_results_summary <- ascaris_data %>% 
  group_by(village,name) %>% 
  summarise(total=n(),
            pos=sum(value,na.rm=TRUE)) %>% 
  mutate(prevalence = map2(pos, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                           broom::tidy())) %>% unnest(prevalence) 
  ggplot(aes(x=village, y=estimate)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1, position=pd) +
  theme_classic() +
  theme(legend.position = 'none',text=element_text(size=40)) +
  scale_y_continuous(labels = scales::percent) +
  #theme(axis.text.x = element_text(hjust = 1)) +
  labs(x='', title='', y='') +
  coord_flip() +
  facet_wrap(~ name)


x <- glm(ascaris_pos ~ Village, data=ascaris_data, family='binomial')
library(margins)
(m <- margins(x))
summary(m)

pacman::p_load(prLogistic)

x <- glmer(ascaris_pos ~ 1 + (1|area_council) + (1|village), data=ascaris_data, family='binomial')
summary(x)
library(sjPlot)
sjPlot::tab_model(x)
sjPlot::plot_model(x)

y <- glmer(ascaris_pos ~ village + (1|area_council), data=ascaris_data, family='binomial')
summary(y)
sjPlot::tab_model(y)


mod_fit <- function(data) {
  glm(value ~ 1 , data=data, family = binomial(link = "logit"))
}


summary(model1)


model <- ascaris_data %>% 
  group_nest(village) %>% 
  mutate(model=map(data,mod_fit)) %>% 
  mutate(
    glance = map(model, broom::glance),
    tidy = map(model, broom::tidy),
    augment = map(model, broom::augment)
  )

model <- snf_results %>% 
  group_nest(village,name) %>% 
  mutate(model=map(data, mod_fit)) %>% 
  mutate(
    glance = map(model, broom::glance),
    tidy = map(model, broom::tidy, exponentiate = TRUE, conf.int = TRUE),
    augment = map(model, broom::augment)
  )

results <- model %>% unnest(tidy)


snf_results %>% 
  # filter(village=='banban_park_labetra' & name=='trichuris_pos') %>% 
  group_by(village,name) %>% 
  summarise(total=n(),
            pos=sum(value,na.rm=TRUE)) %>% 
  mutate(prevalence = map2(pos, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                             broom::tidy())) %>% unnest(prevalence) 
  
  
my.fit <- snf_results %>% filter(village=='assevaia' & name=='trichuris_pos') %>% mutate(value=factor(value)) %>% 
  glm(value ~ 1, data=.,family = binomial(link = "logit")) 

exp(coef(my.fit))  
exp(confint(my.fit))

predictions <- predict(my.fit, type = "response")
predictions  

pacman::p_load(effects)
