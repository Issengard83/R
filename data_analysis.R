### Data analysis for the manuscript: Seroprevalence of Leptospira antibodies 
### in dogs and cats attending to municipal spay/neuter campaigns,
### Santa Fe, Argentina.
### Author: Tamara Ricardo
### Last update: 2023-09-08

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
    MAT_res_n = if_else(MAT_res=="POS", 1, 0),
    N = 1) %>% 
  ### Change reference levels
  mutate_at("breed_cat", fct_relevel, "Mixed breed", after = 0) %>% 
  mutate_at("rodent_sight_fr", fct_relevel, "Always", after = Inf) %>% 
  mutate_at("hous_type", fct_rev) %>% 
  mutate(hous_clean_fr = fct_lump_min(hous_clean_fr, min = 10, other_level = "Occasionally") %>% 
           fct_relevel("Daily", after = 0))

# Load socioeconomic indicators layer -------------------------------------
var_ct = st_read("SHP/VAR_CT_SF.shp") %>% 
  ### Join MAT results
  left_join(imu_df %>% select(redcode, idfrac, adm_dis, MAT_res_n, N)) %>% 
  # Group by census fraction and census tract
  group_by_at(vars(matches("idfrac")|matches("redcode")|
                     matches("lat")|matches("lon")| matches("n_hogares")|
                     starts_with("h_")|starts_with("p"))) %>% 
  # Summarise MAT results by census tract
  summarise(RES = sum(MAT_res_n, na.rm = T),
            N = sum(N, na.rm = T)) %>% 
  # Ungroup data
  ungroup() %>% 
  # Raw indicators as percentages
  mutate_at(vars(starts_with("h_")|matches("pc_hog")), 
            list(pct = ~ round(.*100/n_hogares,2)))

# Descriptive statistics --------------------------------------------------
### Frequencies by animal species
descrTable(animal_sp ~ sex + pregnancies + abort_stillb + 
             age_yrs + age_cat + breed_cat +  BCS_cat +
             vac_any + vac_can_hex + vac_rabies_st + vac_dk_dr +
             MAT_res,  
           max.xlev = 99, method = 4, data = imu_df)


# Create separate dataset for dogs ----------------------------------------
imu_dog = imu_df %>% filter(animal_sp=="Dogs") %>% 
  select(adm_dis, idfrac, age_yrs, sex, breed_cat, BCS_cat,
         starts_with("f"), starts_with("con"), n_dogs_t:rodent_sight_fr, MAT_res)

# Univariate GLMMs for dogs ------------------------------------------------
imu_dog %>% select(-idfrac) %>%  
  tbl_uvregression(y = MAT_res, 
                   method = glmmTMB::glmmTMB,
                   formula = "{y} ~ {x} + (1|adm_dis)",
                   method.args = list(family = binomial),
                   exponentiate = T) %>%
  bold_p()

# Multivariate model ------------------------------------------------------
fit = glmmTMB(MAT_res ~ street_access + vac_dk_dr + con_garbage_dumps + rodent_sight_fr +
                (1|idfrac), 
              family = binomial,
              data = dogs)

### Variable selection
drop1(fit)

fit1 = update(fit, ~.-rodent_sight_fr)
drop1(fit1)

fit2 = update(fit1, ~.-con_garbage_dumps)
drop1(fit2)

fit3 = update(fit2, ~.-vac_dk_dr)
summary(fit3)

# Check model residuals
DHARMa::testResiduals(fit3)

### Coefficients
tbl_regression(fit3, exponentiate = T)

r2(fit)
