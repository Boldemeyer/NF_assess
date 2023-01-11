# Author:Bryce Oldemeyer
# Purpose: Find good examples of good/bad ref reaches for digitizing
# Created: 8/2/2020
# Last Modified:
# Notes: 

#-----------------------------------------------------------------
rm()
#libraries
library(tidyverse)
library(magrittr)
library(sf)
library(here)
library(ggforce)

#-----------------------------------------------------------------
# load data
#-----------------------------------------------------------------


WS_crs = st_crs(4326) # WGS84

cap_path = 'S:/main/data/qrf/DASH_estimates/'

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[1]

cov_choice = c("Reduced","CovLW","Dash","No_elev")[4]

load(paste0(cap_path, mod_choice,'_',cov_choice, '.rda'))

new_preds_sum = new_preds %>%
  mutate(site = trimws(gsub("[[:digit:]]","", site_name)))

###

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[3]

load(paste0(cap_path, mod_choice,'_',cov_choice, '.rda'))

new_preds_win = new_preds %>%
  mutate(site = trimws(gsub("[[:digit:]]","", site_name)))

###

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[2]

load(paste0(cap_path, mod_choice,'_',cov_choice, '.rda'))

new_preds_redd = new_preds %>%
  mutate(site = trimws(gsub("[[:digit:]]","", site_name))) 

new_preds_all = new_preds_sum %>%
  mutate(model ="Juv Summer") %>%
  add_row(., new_preds_win %>%
            mutate(model ="Juv Winter")) %>%
  add_row(., new_preds_redd %>%
            mutate(model = "Redds"))

### Upper NF Summer

Upper_NF = st_read(here('analysis/data/raw_data/DASH/Upper_NF_Digitized_Final.gpkg')) %>%
  st_transform(WS_crs)

Upper_NF_juv_sum<- Upper_NF %>%
  st_join(new_preds_sum)

ggplot(data = Upper_NF_juv_sum) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                        limits = c(0,70),
                        name = "Summer Chinook Parr (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Upper NF")

st_write(Upper_NF_juv_sum,
         dsn = here('analysis/data/derived_data/Upper_NF_juv_sum_dig.gpkg'),
         delete_dsn = T)

### Upper NF Winter

Upper_NF_juv_win<- Upper_NF %>%
  st_join(new_preds_win)

ggplot(data = Upper_NF_juv_win) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.8),
                       name = "Winter Chinook Presmolt (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Upper NF")


st_write(Upper_NF_juv_win,
         dsn = here('analysis/data/derived_data/Upper_NF_juv_win_dig.gpkg'),
         delete_dsn = T)

### Upper NF Redds

Upper_NF_redd<- Upper_NF %>%
  st_join(new_preds_redd)

ggplot(data = Upper_NF_redd) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.08),
                       name = "Chinook Redds (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Upper NF")

##### Arbon

Arbon = st_read(here('analysis/data/raw_data/DASH/Arbon_Digitized_Final.gpkg')) %>%
  st_transform(WS_crs)

### Arbon winter

Arbon_juv_win<- Arbon %>%
  st_join(new_preds_win) 

ggplot(data = Arbon_juv_win) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.4),
                       name = "Winter Chinook Presmolt (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Arbon")

st_write(Arbon_juv_win,
         dsn = here('analysis/data/derived_data/Arbon_juv_win_dig.gpkg'),
         delete_dsn = T)


### Grouse redd

grouse = st_read(here('analysis/data/raw_data/DASH/Grouse_Digitized_Final.gpkg')) %>%
  st_transform(WS_crs)

grouse_redd<- grouse %>%
  st_join(new_preds_redd) 

ggplot(data = grouse_redd) +
  geom_sf(aes(fill = sthd_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.09),
                       name = "Chinook Redd (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Grouse")

st_write(grouse_redd,
         dsn = here('analysis/data/derived_data/Grouse_redd_dig.gpkg'),
         delete_dsn = T)

#####Corduroy

corduroy = st_read(here('analysis/data/raw_data/Corduroy digitized FINAL.gpkg')) %>%
  st_transform(WS_crs)

corduroy_juv_sum<- corduroy %>%
  st_join(new_preds_sum)

### Cache Summer
ggplot(data = corduroy_juv_sum) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,12.8),
                       name = "Summer Chinook Parr (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Corduroy, Elk Creek")

### Cache winter

corduroy_juv_win<- corduroy %>%
  st_join(new_preds_win) %>%
  fill(site_name.y:site_group_ws, .direction = 'up')

ggplot(data = corduroy_juv_win) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.6),
                       name = "Winter Chinook Presmolt (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Corduroy, Elk Creek")

st_write(corduroy_juv_win,
         dsn = here('analysis/data/derived_data/Corduroy_juv_win.gpkg'),
         delete_dsn = T)


### Cache redd

corduroy_redd<- corduroy %>%
  st_join(new_preds_redd) %>%
  fill(site_name.y:site_group_ws, .direction = 'up')

ggplot(data = corduroy_redd) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.018),
                       name = "Chinook Redd (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Corduroy, Elk Creek")

### Lower Lemhi MRA
ll_mra = st_read(here('analysis/data/raw_data/Lower Lemhi MRA digitized FINAL.gpkg')) %>%
  st_transform(WS_crs)

### Lower Lemhi MRA - winter
ll_mra_juv_win = ll_mra %>%
  st_join(new_preds_win)

ll_mra_p = ggplot(data = ll_mra_juv_win) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.6),
                       name = "Winter Chinook Presmolt (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Lower Lemhi MRA")
ll_mra_p

st_write(ll_mra_juv_win,
         dsn = here('analysis/data/derived_data/Lower_Lemhi_MRA_juv_win.gpkg'),
         delete_dsn = T)

# Summer juvenile sites

# Bad 
# Kenney Cr - Lemhi
# Lower Pahsimeroi MRA - Pahsimeroi
# Decker Flat - Upper Salmon

# Good
# Grouse, Lake, Summit - Secesh
# Cache Money, BV CG,  - Bear Valley

# Marsh - Marsh (not great)

