# Authors: Bryce Oldemeyer, original script Mike Ackerman 
# Purpose: Prep QRF and extrapolation model covariate tables from QRFcapacity repo
# Created: 4/10/2022
# Last Modified: 4/11/2022
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(magrittr)
library(here)

#-----------------------------------------------------------------
# set NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {nas_prefix = "S:"}

#-----------------------------------------------------------------
# load QRF model fits
load("C:/Users/bolde/Documents/Git/QRFCapacity/output/modelFits/qrf_juv_summer.rda")
juv_sum_chnk_covs = qrf_mods$Chinook$importance
juv_sum_sthd_covs = qrf_mods$Steelhead$importance


load("C:/Users/bolde/Documents/Git/QRFCapacity/output/modelFits/qrf_juv_winter.rda")
juv_win_chnk_covs = qrf_mods$Chinook$importance
juv_win_sthd_covs = qrf_mods$Steelhead$importance

load("C:/Users/bolde/Documents/Git/QRFCapacity/output/modelFits/qrf_redds.rda")
redds_chnk_covs = qrf_mods$Chinook$importance
redds_sthd_covs = qrf_mods$Steelhead$importance

# clean up environment
rm(fish_hab,
   hab_avg,
   hab_data,
   qrf_mod_df,
   qrf_mods,
   sel_hab_mets)

# Create Habitat Covariate Table
# Juvenile Summer Chinook
hab_cov_tbl = as_tibble(juv_sum_chnk_covs, rownames = "Covariate") %>%
  mutate(`Juv Sum Chnk` = dense_rank(desc(IncNodePurity))) %>%
  select(-IncNodePurity) %>%
  # Juvenile Summer Steelhead
  full_join(as_tibble(juv_sum_sthd_covs, rownames = "Covariate") %>%
            mutate(`Juv Sum Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Juvenile Winter Chinook
  full_join(as_tibble(juv_win_chnk_covs, rownames = "Covariate") %>%
            mutate(`Juv Win Chnk` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Juvenile Winter Steelhead
  full_join(as_tibble(juv_win_sthd_covs, rownames = "Covariate") %>%
            mutate(`Juv Win Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Redds Chinook
  full_join(as_tibble(redds_chnk_covs, rownames = "Covariate") %>%
            mutate(`Redds Chnk` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Redds Steelhead
  full_join(as_tibble(redds_sthd_covs, rownames = "Covariate") %>%
            mutate(`Redds Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  left_join(hab_dict %>%
              select(ShortName,
                     Name,
                     DescriptiveText,
                     MetricCategory),
            by = c("Covariate" = "ShortName")) %>%
  distinct() %>%
  # some cleaning
  mutate(`Juv Win Chnk` = ifelse(Covariate == "Q", 2, `Juv Win Chnk`),
         `Juv Win Sthd` = ifelse(Covariate == "Q", 1, `Juv Win Sthd`),
         `Juv Win Chnk` = ifelse(Covariate == "LWFreq_Wet", 10, `Juv Win Chnk`),
         `Juv Win Sthd` = ifelse(Covariate == "LWFreq_Wet", 10, `Juv Win Sthd`)) %>%
  filter(!Covariate %in% c("Discharge", "LWCount")) %>%
  select(Covariate,
         Name,
         MetricCategory,
         everything(),
         DescriptiveText) %>%
  mutate(
    MetricCategory = if_else(Covariate == "DpthResid" & is.na(MetricCategory), "Size", MetricCategory),
    MetricCategory = if_else(Covariate == "Tier1" & is.na(MetricCategory), "ChannelUnit", MetricCategory)
  ) %>%
  arrange(MetricCategory,
          Covariate) %>%
  rename(`Metric Category` = MetricCategory,
         Description = DescriptiveText)

# save results
save(hab_cov_tbl,
     file = here("C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/hab_cov_tbl.rda"))

# GAA table
gaa_tbl = tibble(
  Metric = c("slope",
             "rel_slope",
             "Sinuosity",
             "regime",
             "alp_accum",
             "fines_accu",
             "flow_accum",
             "grav_accum",
             "p_accum",
             "fp_cur",
             "S2_02_11",
             "DistPrin1",
             "NatPrin1",
             "NatPrin2"),
  Description = c(
    "Stream gradient (%).",
    "Relative slope. Reach slope minus upstream slope.",
    "Reach sinuosity. 1 = straight, 1 < sinuous.",
    "Flow regime. 1 = mixed. 2 = snow dominated, 3 = rain dominated.",
    "Number of upstream cells in alpine terrain.",
    "Number of upstream cells in fine grain lithologies.",
    "Number of upstream DEM cells flowing into reach.",
    "Number of upstream cells in gravel producing lithologies.",
    "Number of upstream cells weighted by average annual precipitation.",
    "Current unmodified floodplain width.",
    "Historical composite scenario representing 10 year average August mean stream temperatures for 2002-2011 (Isaak et al. 2017).",
    "Disturbance Classification PCA 1 Score (Whittier et al. 2011).",
    "Natural Classification PCA 1 Score (Whittier et al. 2011).",
    "Natural Classification PCA 2 Score (Whittier et al. 2011)."
    ))

# save results
save(gaa_tbl,
     file = here("C:/Users/bolde/Documents/Git/NF_assess/analysis/data/derived_data/extrap_gaa_tbl.rda"))




