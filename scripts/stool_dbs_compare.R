library(tidyverse)
library(here)
library(janitor)
compare_raw <- readxl::read_xlsx(here('data', 'input', 'compare_sanma_tafea.xlsx'))


pd = position_dodge(0.5)

(stool <- compare_raw %>% pivot_longer(-Indicator) %>% 
  pivot_wider(names_from=Indicator, values_from=value) %>% 
  clean_names() %>% 
  select(name, people_registered,stool_samples_collected) %>% 
  mutate(alb_prop = map2(stool_samples_collected, people_registered, ~ prop.test(.x, .y, conf.level=0.95) %>%
                           broom::tidy())) %>% unnest(alb_prop) %>% 
  mutate(name=factor(name, levels=c('Tafea', 'Sanma'))) %>% 
ggplot(aes(x=name, y=estimate)) +
    geom_point(position=pd, size=8) +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1, position=pd, size=4) +
    theme_classic() +
  theme(legend.position = 'none',text=element_text(size=40)) +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.3), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30)) +
#theme(axis.text.x = element_text(hjust = 1)) +
  labs(x='', title='', y='') 
)

(dbs <- compare_raw %>% pivot_longer(-Indicator) %>% 
  pivot_wider(names_from=Indicator, values_from=value) %>% 
  clean_names() %>% 
  select(name, people_registered,dried_blood_spots) %>% 
  mutate(alb_prop = map2(dried_blood_spots, people_registered, ~ prop.test(.x, .y, conf.level=0.95) %>%
                           broom::tidy())) %>% unnest(alb_prop) %>% 
  mutate(name=factor(name, levels=c('Tafea', 'Sanma'))) %>% 
  ggplot(aes(x=name, y=estimate)) +
  geom_point(position=pd, size=8) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1, position=pd, size=4) +
  theme_classic() +
  theme(legend.position = 'none',
        text=element_text(size=40)) +
  #scale_y_continuous(labels = scales::percent, limits=c(0,NA)) +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.3), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30)) +
  labs(x='', title='', y='') 
)

library(patchwork)
stool + dbs
