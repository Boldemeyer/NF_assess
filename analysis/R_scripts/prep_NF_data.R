# Authors: Bryce Oldemeyer, orig script MA & KS
# Purpose: Prep data to be used in the NF assessment
# Created: 4/07/2020
# Last Modified: --
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(sf)
library(here)
library(tidyverse)
library(magrittr)
library(ggmap)
library(nngeo)
library(rgdal)

#-----------------------------------------------------------------
# set NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {nas_prefix = "S:"}

# set default crs
NF_crs = st_crs(4326) # WGS84

#-----------------------------------------------------------------
# read in HUC12 watershed boundaries from NAS, majority of PNW
huc12_sf = st_read(paste0(nas_prefix, "main/data/habitat/watershed_boundaries/WBDHU12.shp")) %>%
  st_transform(NF_crs)

#-----------------------------------------------------------------
# read in current QRF extrapolations, for entire CRB
# use Morgan Bond's spatially continuous, 200m linear network layer
# use random forest extrapolation model

# sum_juv_sf = st_read(paste0(nas_prefix, "main/data/qrf/extrapolations/Rch_Cap_RF_juv_summer_dash.gpkg")) %>% st_transform(ww_crs)
# win_juv_sf = st_read(paste0(nas_prefix, "main/data/qrf/extrapolations/Rch_Cap_RF_juv_winter.gpkg")) %>% st_transform(ww_crs)
# redd_sf = st_read(paste0(nas_prefix, "main/data/qrf/extrapolations/Rch_Cap_RF_redds.gpkg")) %>% st_transform(ww_crs)

# if needed, these .gpkg files can be saved locally to decrease read times, e.g.,
# on Mike's machine...

sum_juv_sf = st_read("C:/Users/bolde/Documents/Git/QRF_extrapolations/Rch_Cap_RF_juv_summer_dash.gpkg") %>%
  st_transform(NF_crs)
win_juv_sf = st_read("C:/Users/bolde/Documents/Git/QRF_extrapolations/Rch_Cap_RF_juv_winter.gpkg") %>%
  st_transform(NF_crs)
redd_sf = st_read("C:/Users/bolde/Documents/Git/QRF_extrapolations/Rch_Cap_RF_redds.gpkg") %>%
  st_transform(NF_crs)

# these are too large to reasonably plot

#-----------------------------------------------------------------
# Only want NF Salmon, and compute total capacity at each reach
# note that this does NOT filter for species ranges, yet
NF_huc_sf = st_read(here("C:/Users/bolde/Documents/Git/NF_assess/analysis/data/raw_data/watershed_boundary/NF_Watershed_Boundary.shp")) %>%
  st_transform(NF_crs) %>%
  mutate(HUC8 = str_sub(HUC12, 1, 8),
         HUC10 = str_sub(HUC12, 1, 10))

# plot NF_huc_sf
ggplot(data = NF_huc_sf) +
  geom_sf()

#-----------------------------------------------------------------
# filter WW QRF extrapolations

# summer parr
NF_sum_sf = sum_juv_sf %>%
  st_intersection(NF_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(NF_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(NF_sum_sf) + geom_sf()

# winter presmolt
NF_win_sf = win_juv_sf %>%
  st_intersection(NF_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate winter, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(NF_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(NF_win_sf) + geom_sf()

# redds
NF_redd_sf = redd_sf %>%
  st_intersection(NF_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate redd capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(NF_huc_sf %>%
            select(NAME,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(NF_redd_sf) + geom_sf()

#-----------------------------------------------------------------
# get 200m reach layer for this area; contains a bunch of habitat metrics for the 200m reaches
# this dataset is in the QRFcapacity repo
load("C:/Users/bolde/Documents/Git/QRFCapacity/data/rch_200.rda")

# alternatively, by installing QRFcapacity R package...
# library(devtools)
# devtools::install_github("KevinSee/QRFcapacity")
# library(QRFcapacity)
# data("rch_200")

# trim down 200m reach layer
NF_rch_sf = rch_200 %>%
  filter(UniqueID %in% NF_sum_sf$UniqueID) %>%
  st_transform(NF_crs)

ggplot(NF_rch_sf) + geom_sf()

# compare with the stream network provided by CTUIR, this is for entire Walla Walla
#ctuir_rch_sf = st_read(here("analysis/data/raw_data/stream_network/CTUIRStillwaterStreamNetwork.shp")) %>%
#  st_transform(uww_crs)

#ggplot(ctuir_rch_sf) + geom_sf()

# read in UWW base referencing geodatabase
#uww_gdb_filepath = here("analysis/data/raw_data/stream_network/UWW_base_referencing.gdb")
#uww_gdb_layers = ogrListLayers(uww_gdb_filepath)

# extract UWW_streams_24k layer from geodatabase
#uww_base_ref_streams_24k  = readOGR(dsn = uww_gdb_filepath,
#                                   layer = "UWW_streams_24k") %>%
#  st_as_sf() %>%
#  st_transform(uww_crs)

#ggplot(uww_base_ref_streams_24k) + geom_sf()
# Thankfully, the base reference streams and our rch_200 appear to be the same linear networks!!!

#-----------------------------------------------------------------
# save data for this repository
save(NF_sum_sf,
     NF_win_sf,
     NF_redd_sf,
     file = "C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/NF_qrf_extrapolations.rda")

save(NF_huc_sf,
     NF_rch_sf,
     file = "C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/NF_spatial.rda")

#-----------------------------------------------------------------
# save geopackages for use in QGIS
st_write(NF_sum_sf,
         dsn = "C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/NF_juv_sum_qrf.gpkg",
         append = F)
st_write(NF_win_sf,
         dsn = "C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/NF_juv_win_qrf.gpkg",
         append = F)
st_write(NF_redd_sf,
         dsn = "C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/NF_redd_qrf.gpkg",
         append = F)
st_write(NF_rch_sf,
         dsn = "C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/NF_rch_200.gpkg",
         append = F)
st_write(NF_huc_sf,
         dsn = "C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/NF_huc_bndry.gpkg",
         append = F)
st_write(NF_base_ref_streams_24k,
         dsn = "C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/NF_base_ref_streams.gpkg",
         append = F)

#-----------------------------------------------------------------
# just a preliminary plot

# pick a river color
river_color = "lightskyblue1"

NF_sum_sf %>%
  ggplot() +
  geom_sf(data = NF_huc_sf %>%
            st_union() %>%
            nngeo::st_remove_holes(),
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = NF_sum_sf %>%
            filter(chnk), # only records in the StreamNet Chinook domain
          aes(color = chnk_per_m2),
          size = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_color_viridis_c(direction = -1) +
  labs(title = "North Fork Salmon Watershed",
       color = expression(`Chinook Parr` / m^2))
