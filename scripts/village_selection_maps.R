library(sf)
library(here)
library(ggsflabel)
library(cowplot)
library(tidyverse)

# vanautu_map_fn <- function(x) {
van_map_loc <- list.files(here('data', 'input', 'shp', 'vut_admbnda_adm2_spc_20180824'), pattern = 'shp$', full.names = T, recursive=T)
van_map <- read_sf(van_map_loc) %>% mutate(province=factor(ADM1_EN, 
                                                           levels=c('Torba',
                                                                    'Sanma',
                                                                    'Penama',
                                                                    'Malampa',
                                                                    'Shefa',
                                                                    'Tafea')))
sf::sf_use_s2(FALSE)

van_adm1_sf <- van_map %>% 
  group_by(province) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>% 
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))


# SANMA -------------------------------------------------------------------
select_sf <- van_map %>% 
  filter(province == 'Sanma')

bounding_box = st_as_sfc(st_bbox(select_sf))

(vanuatu_map <- ggplot(data=van_adm1_sf) +
  geom_sf(aes(fill=province)) +
  geom_sf(data = bounding_box, fill = NA, color = "red", size = 1.2) +
  #theme_bw(base_family = "Roboto Condensed") +
    theme_void() +
  theme(legend.position = 'left', text = element_text(size = 40)) +
  labs(fill='Province') )

# get COUNTY data for a given state
counties_spec <- select_sf %>% 
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) # add centroid values for labels

village_raw <- readxl::read_xlsx(here('data', 'input', 'SANMA_selected villages_21Aug22.xlsx'))

village_processed <- village_raw %>% janitor::clean_names() %>% 
  filter(!is.na(latitude)) %>% filter(latitude!='NO MATCH') %>%  
  st_as_sf(coords=c('latitude', 'longitude')) %>% 
  st_set_crs(st_crs(van_map))

(survey_village_map <- ggplot(data=select_sf) +
  geom_sf(aes()) +
  geom_sf(data=village_processed, col='red', size=4) +
    ggsn::scalebar(select_sf, dist = 25, dist_unit = "km",location='bottomleft',
                   transform = TRUE, model = "WGS84") +
  #scale_fill_brewer(palette = "Paired") +
  #geom_sf_label_repel(data=village_processed, aes( label=v_name),nudge_x = +0.3, nudge_y = +0.2) +
  #geom_sf_label_repel(data=counties_spec, aes(x=lon, y=lat, label=ADM2_EN),nudge_x = +0.2, nudge_y = +0.1) +
  labs(fill='') +
  theme_bw(base_family = "Roboto Condensed") +
  theme(legend.position = 'none') + theme_void()
)

(two_maps <- ggdraw() +
  draw_plot(vanuatu_map,x = 0, y = 0.15, width = 0.5, height = 0.8) +
  draw_plot(survey_village_map,x = 0.45, y = 0.15, width = 0.5, height = 0.8) 
)

# TAFEA -------------------------------------------------------------------

select_sf <- van_map %>% 
  filter(province == 'Tafea')

bounding_box = st_as_sfc(st_bbox(select_sf))

(vanuatu_map <- ggplot(data=van_adm1_sf) +
    geom_sf(aes(fill=province)) +
    geom_sf(data = bounding_box, fill = NA, color = "red", size = 1.2) +
    #theme_bw(base_family = "Roboto Condensed") +
    theme_void() +
    theme(legend.position = 'left', text = element_text(size = 40)) +
    labs(fill='Province') )

# get COUNTY data for a given state
counties_spec <- select_sf %>% 
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) # add centroid values for labels

village_raw <- readxl::read_xlsx(here('data', 'input', 'TAFEA_selected villages_28Oct2022.xlsx'))

village_processed <- village_raw %>% janitor::clean_names() %>% 
  filter(!is.na(latitude)) %>% filter(latitude!='NO MATCH') %>%  
  st_as_sf(coords=c('latitude', 'longitude')) %>% 
  st_set_crs(st_crs(van_map))

(survey_village_map <- ggplot(data=select_sf) +
  geom_sf(aes()) +
  geom_sf(data=village_processed, col='red', size=2, alpha=0.6) +
  ggsn::scalebar(select_sf, dist = 25, dist_unit = "km",location='bottomleft',
                 transform = TRUE, model = "WGS84") +
  #scale_fill_brewer(palette = "Paired") +
  #geom_sf_label_repel(data=village_processed, aes( label=v_name),nudge_x = +0.3, nudge_y = +0.2) +
  #geom_sf_label_repel(data=counties_spec, aes(x=lon, y=lat, label=ADM2_EN),nudge_x = +0.2, nudge_y = +0.1) +
  labs(fill='') +
  theme_bw(base_family = "Roboto Condensed") +
  theme(legend.position = 'none') + theme_void()
)

(two_maps <- ggdraw() +
    draw_plot(vanuatu_map,x = 0, y = 0.15, width = 0.5, height = 0.8) +
    draw_plot(survey_village_map,x = 0.45, y = 0.15, width = 0.5, height = 0.8) 
)


# Shefa -------------------------------------------------------------------

select_sf <- van_map %>% 
  filter(province == 'Shefa')

bounding_box = st_as_sfc(st_bbox(select_sf))

(vanuatu_map <- ggplot(data=van_adm1_sf) +
    geom_sf(aes(fill=province)) +
    geom_sf(data = bounding_box, fill = NA, color = "red", size = 1.2) +
    #theme_bw(base_family = "Roboto Condensed") +
    theme_void() +
    theme(legend.position = 'left', text = element_text(size = 40)) +
    labs(fill='Province') )

# get COUNTY data for a given state
counties_spec <- select_sf %>% 
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) # add centroid values for labels

village_raw <- readxl::read_xlsx(here('data', 'input', 'SHEFA_selected villages_18Oct2022.xlsx'))

village_processed <- village_raw %>% janitor::clean_names() %>% 
  filter(!is.na(latitude)) %>% filter(latitude!='NO MATCH') %>%  
  st_as_sf(coords=c('latitude', 'longitude')) %>% 
  st_set_crs(st_crs(van_map))

(survey_village_map <- ggplot(data=select_sf) +
  geom_sf(aes()) +
  geom_sf(data=village_processed, col='red', size=4) +
  ggsn::scalebar(select_sf, dist = 25, dist_unit = "km",location='bottomleft',
                 transform = TRUE, model = "WGS84") +
  #scale_fill_brewer(palette = "Paired") +
  #geom_sf_label_repel(data=village_processed, aes( label=v_name),nudge_x = +0.3, nudge_y = +0.2) +
  #geom_sf_label_repel(data=counties_spec, aes(x=lon, y=lat, label=ADM2_EN),nudge_x = +0.2, nudge_y = +0.1) +
  labs(fill='') +
  theme_bw(base_family = "Roboto Condensed") +
  theme(legend.position = 'none') + theme_void()
)

(two_maps <- ggdraw() +
    draw_plot(vanuatu_map,x = 0, y = 0.15, width = 0.5, height = 0.8) +
    draw_plot(survey_village_map,x = 0.45, y = 0.15, width = 0.5, height = 0.8) 
)


# All survey provinces ----------------------------------------------------
shefa_sf <- van_map %>% 
  filter(province == 'Shefa')

shefa_bounding_box = st_as_sfc(st_bbox(shefa_sf))

tafea_sf <- van_map %>% 
  filter(province == 'Tafea')

tafea_bounding_box = st_as_sfc(st_bbox(tafea_sf))

sanma_sf <- van_map %>% 
  filter(province == 'Sanma')

sanma_bounding_box = st_as_sfc(st_bbox(sanma_sf))

van_counties_spec <- van_adm1_sf %>% 
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  mutate(x_nudge=case_when(province=='Torba' ~ 1, 
                           province=='Sanma' ~ -400, 
                           province=='Penama' ~ 2, 
                           province=='Tafea' ~ -3, 
                           #province=='Malampa' ~ -8, 
                          TRUE ~ -5))

(vanuatu_map <- ggplot(data=van_adm1_sf) +
    geom_sf(aes(fill=province)) +
    geom_sf(data = shefa_bounding_box, fill = NA, color = "red", size = 1.2) +
    geom_sf(data = tafea_bounding_box, fill = NA, color = "red", size = 1.2) +
    geom_sf(data = sanma_bounding_box, fill = NA, color = "red", size = 1.2) +
    # geom_sf_label_repel(inherit.aes=FALSE,data=counties_spec, aes(x=lon, y=lat, label=province), size=10,
    #                     nudge_x = van_counties_spec$x_nudge,
    #                     nudge_y = van_counties_spec$y_nudge) +
    ggsn::scalebar(van_adm1_sf, dist = 50, dist_unit = "km",location='bottomleft',
                   transform = TRUE, model = "WGS84") +
    theme_void() +
    theme(legend.position = 'none', text = element_text(size = 40)) +
    labs(fill='Province') )


