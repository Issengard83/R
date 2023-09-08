### Data analysis for the manuscript: Seroprevalence of Leptospira antibodies 
### in dogs and cats attending to municipal spay/neuter campaigns,
### Santa Fe, Argentina.
### Author: Tamara Ricardo
### Last update: 2023-09-07

# LOAD PACKAGES -----------------------------------------------------------
pacman::p_load(
  # Descriptive statistics
  gtsummary,
  compareGroups,
  # Model fit
  glmmTMB,
  mgcv,
  # Residuals and inference
  performance,
  # Map tools
  sf, 
  OpenStreetMap,
  # ggmap,
  # Colorblind-friendly palettes
  scico,
  # Data management
  tidyverse,
  janitor)

# Load IMUSA dataset ------------------------------------------------------
imu_df = readxl::read_excel("data_IMUSA_clean.xlsx") %>% 
  mutate(
    ### All strings to factors
    across(where(is.character)|matches("redcode")|matches("idfrac"), as.factor),
    ### Results to numeric
    RES = if_else(res_MAT=="POS", 1, 0),
    N = 1)

# Load socioeconomic indicators layer -------------------------------------
var_ct = st_read("SHP/VAR_CT_SF.shp") %>% 
  ### Join MAT results
  left_join(imu_df %>% select(redcode,idfrac,adm_district,RES,N)) %>% 
  # Group by census fraction and census tract
  group_by_at(vars(matches("idfrac")|matches("redcode")|
                     matches("lat")|matches("lon")| matches("n_hogares")|
                     starts_with("h_")|starts_with("p"))) %>% 
  # Summarise MAT results by census tract
  summarise(RES = sum(RES, na.rm = T),
            N = sum(N, na.rm = T)) %>% 
  # Ungroup data
  ungroup() %>% 
  # Raw indicators as percentages
  mutate_at(vars(starts_with("h_")|matches("pc_hog")), 
            list(pct = ~ round(.*100/n_hogares,2)))

# Descriptive statistics --------------------------------------------------
### Frequencies by animal species
descrTable(animal_species ~ res_MAT + 
             sex + pregnancies + abortions_stillbirths + 
             age_yrs + age_cat + breed_cat +  BCS_cat +
             vac_any + vac_rabies_st + vac_dk_dr,  
           max.xlev = 99, method = 4, data = imu_df)

### Samples per census fraction
fct_count(imu_df$idfrac) %>% 
  arrange(desc(n))

### Samples per administrative district
fct_count(imu_df$adm_district) %>% 
  arrange(desc(n))

# Table 1: serovar-specific seroprevalence --------------------------------
descrTable(animal_species ~ res_MAT + AUS + AUT + CAN + CAS + COP + 
             GRI + HAR + POM + PYR + TAR, data = imu_df)
