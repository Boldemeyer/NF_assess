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

site_cats = read_csv(here('analysis/data/SiteList_ForMA.csv'))

new_preds_sum = new_preds %>%
  mutate(site = trimws(gsub("[[:digit:]]","", site_name))) %>%
  left_join(site_cats)

###

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[3]

load(paste0(cap_path, mod_choice,'_',cov_choice, '.rda'))

new_preds_win = new_preds %>%
  mutate(site = trimws(gsub("[[:digit:]]","", site_name))) %>%
  left_join(site_cats)

###

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[2]

load(paste0(cap_path, mod_choice,'_',cov_choice, '.rda'))

new_preds_redd = new_preds %>%
  mutate(site = trimws(gsub("[[:digit:]]","", site_name))) %>%
  left_join(site_cats)

new_preds_all = new_preds_sum %>%
  mutate(model ="Juv Summer") %>%
  add_row(., new_preds_win %>%
            mutate(model ="Juv Winter")) %>%
  add_row(., new_preds_redd %>%
            mutate(model = "Redds"))

### BV_CG Summer

BV_CG = st_read(here('analysis/data/raw_data/Bear Valley CG digitized FINAL.gpkg')) %>%
  st_transform(WS_crs)

BV_CG_juv_sum<- BV_CG %>%
  st_join(new_preds_sum)

ggplot(data = BV_CG_juv_sum) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                        limits = c(0,12.8),
                        name = "Summer Chinook Parr (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Bear Valley CG, Bear Valley")

### BV_CG Winter

BV_CG_juv_win<- BV_CG %>%
  st_join(new_preds_win)

ggplot(data = BV_CG_juv_win) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.4),
                       name = "Winter Chinook Presmolt (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Bear Valley CG, Bear Valley")


st_write(BV_CG_juv_win,
         dsn = here('analysis/data/derived_data/Bear_valley_CG_juv_win.gpkg'),
         delete_dsn = T)

### BV_CG Redds

BV_CG_redd<- BV_CG %>%
  st_join(new_preds_redd)

ggplot(data = BV_CG_redd) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.0045),
                       name = "Chinook Redds (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Bear Valley CG, Bear Valley")

#####Cache

cache_money = st_read(here('analysis/data/raw_data/Cache Money digitized FINAL.gpkg')) %>%
  st_transform(WS_crs)

cache_money_juv_sum<- cache_money %>%
  st_join(new_preds_sum) %>%
  fill(site_name.y:site_group_ws, .direction = 'up')

### Cache Summer
ggplot(data = cache_money_juv_sum) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,12.8),
                       name = "Summer Chinook Parr (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Cache Money, Bear Valley")

### Cache winter

cache_money_juv_win<- cache_money %>%
  st_join(new_preds_win) %>%
  fill(site_name.y:site_group_ws, .direction = 'up')

ggplot(data = cache_money_juv_win) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.4),
                       name = "Winter Chinook Presmolt (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Cache Money, Bear Valley")


### Cache redd

cache_money_redd<- cache_money %>%
  st_join(new_preds_redd) %>%
  fill(site_name.y:site_group_ws, .direction = 'up')

ggplot(data = cache_money_redd) +
  geom_sf(aes(fill = chnk_per_m)) +
  scale_fill_viridis_c(direction = -1,
                       limits = c(0,0.018),
                       name = "Chinook Redd (per m)") +
  theme(axis.text = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Cache Money, Bear Valley")


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

