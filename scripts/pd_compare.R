library(tidyverse)
library(here)

lab_results <- readxl::read_xlsx(here('data', 'input', 'snf_results.xlsx'))

geom_errorbar(aes(ymin=len-ci, ymax=len+ci), colour="black", width=.1, position=pd) 
  

pd <- position_dodge(0.1) # move them .05 to the left and right

lab_results %>% pivot_wider(names_from='INDICATOR', values_from='VALUE') %>% 
  mutate(PATHOGEN=factor(PATHOGEN, levels=c('ASCARIS', 'TRICHURIS', 'HOOKWORM'))) %>% 
  #filter(ISLAND=="ALL") %>% 
  ggplot(aes(x=PD, y=PROPORTION)) +
  geom_point() +
  geom_errorbar(aes(ymin=LOWER, ymax=UPPER), colour="black", width=.1, position=pd) +
  theme_classic() +
  labs(x='PD IN FIELD') +
  facet_wrap(~ ISLAND + PATHOGEN)
