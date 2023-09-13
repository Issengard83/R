### Data analysis for the manuscript: Seroprevalence of Leptospira antibodies 
### in dogs and cats attending to municipal spay/neuter campaigns,
### Santa Fe, Argentina.
### Author: Tamara Ricardo
### Last update: 2023-09-13

# LOAD PACKAGES -----------------------------------------------------------
pacman::p_load(
  # Descriptive statistics
  gtsummary,
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
  janitor,
  flextable)

# Load IMUSA dataset ------------------------------------------------------
imu_df = readxl::read_excel("data_IMUSA_clean.xlsx") %>% 
  mutate(
    ### All strings to factors
    across(where(is.character)|matches("redcode")|matches("idfrac"), as.factor),
    ### Overall number of dogs and cats in the household
    n_pets_t = rowSums(select(.,n_dogs_t, n_cats_t))) %>% 
  ### Change reference levels
  mutate_at("breed_cat", fct_relevel, "Mixed breed", after = 0) %>% 
  mutate_at("rodent_sight_fr", fct_relevel, "Always", after = Inf) %>% 
  mutate_at("animal_fun", fct_relevel, "Companion only", after = 0) %>% 
  mutate_at(c("animal_feed","hous_type"), fct_rev) %>% 
  mutate(hous_clean_fr = fct_lump_min(hous_fr_clean, min = 10, other_level = "Occasionally") %>% 
           fct_relevel("Daily", after = 0))

# Load socioeconomic indicators layer -------------------------------------
var_ct = st_read("SHP/VAR_CT_SF.shp") %>% 
  ### Join MAT results
  left_join(imu_df %>% group_by(redcode) %>% 
              summarise(POS = if_else(MAT_res=="POS", 1, 0) %>% sum(., na.rm = T),
                        N = n())) %>% 
  ungroup() %>% 
  # Raw indicators as percentages
  mutate_at(vars(starts_with("h_")|matches("pc_hog")), 
            list(pct = ~ round(.*100/n_hogares,2)))

### Number of samples per district
tbl_summary(imu_df, include = adm_dis, 
            sort = list(everything() ~ "frequency"))

# Table 1: descriptive statistics -----------------------------------------
tbl_summary(imu_df, by = animal_sp,
            include = c(age_yrs, age_cat, sex, pregnancies, abort_still, breed_cat,
                        BCS_cat, animal_fun, animal_feed,
                        vac_any, vac_can_hex, vac_rabies_st, vac_dk_dr),
            missing = "no",
            label = list(
              "age_yrs" ~ "Age (years)",
              "age_cat" ~ "Age group",
              "sex" ~ "Sex",
              "pregnancies" ~ "Pregnancies",
              "abort_still" ~ "Abortions/stillbirths",
              "breed_cat" ~ "Breed",
              "BCS_cat" ~ "BCS",
              "animal_fun" ~ "Role/s of the animal",
              "animal_feed" ~ "Type of feeding",
              "vac_any" = "Received any vaccine",
              "vac_can_hex" = "Canine hexavalent vaccine (>6 months)",
              "vac_rabies_st" = "Rabies vaccine at sterilization",
              "vac_dk_dr" = "Vaccines DK/DR"
              )) %>% 
  add_p() %>% 
  bold_p() 
# %>% 
#   as_flex_table()  %>% 
#   save_as_docx(path = "tab1.docx")


# Table 2: seropositivity by serovar and species --------------------------
imu_df %>%  select(animal_sp, Australis:Tarassovi) %>% 
  pivot_longer(cols = -animal_sp, names_to = "Serovar", values_to = "RES") %>% 
  filter(RES=="Yes") %>%
  tbl_summary(by = animal_sp, include = -RES) 
# %>%
#   as_flex_table()  %>%
#   save_as_docx(path = "tab2.docx")

# Create separate dataset for dogs ----------------------------------------
dog_df = imu_df %>% filter(animal_sp=="Dog") %>% 
  select(adm_dis, age_yrs, sex, breed_cat, BCS_cat, animal_feed, animal_fun,
         street_access, starts_with("con"), n_dogs_t:rodent_sight_fr, MAT_res)


# Table 3: univariate GLMMs for dogs --------------------------------------
dog_df %>% tbl_uvregression(y = MAT_res, 
                   method = glmmTMB::glmmTMB,
                   formula = "{y} ~ {x} + (1|adm_dis)",
                   method.args = list(family = binomial),
                   exponentiate = T,
                   show_single_row = starts_with("street")|
                     starts_with("con")|
                     starts_with("hunt")|
                     starts_with("mud"),
                   label = list(age_yrs ~ "Age (years)",
                                sex ~ "Sex",
                                breed_cat ~ "Breed categor",
                                BCS_cat ~ "BCS",
                                animal_feed ~ "Type of feeding",
                                animal_fun ~ "Role/s of the animal",
                                street_access ~ "Street access",
                                con_garbage_dumps ~ "Contact w/ garbage dumps",
                                con_mud_water ~ "Wades in mud/water",
                                hunt_animals ~ "Hunting behavior",
                                hunt_rodents ~ "Hunts rodents:",
                                hunt_wild_anim ~ "Hunts wild animals",
                                con_dogs ~ "Contact w/dogs",
                                con_cats ~ "Contact w/cats",
                                con_anim_others ~ "Contact w/other animals",
                                con_anim_neighbors ~ "Contact w/neighbor pets",
                                con_anim_house ~ "Contact w/house pets",
                                con_anim_stray ~ "Contact w/stray animals",
                                n_dogs_t ~ "Number of owned dogs",
                                n_cats_t ~ "Number of owned cats",
                                hous_type ~ "Type of housing",
                                hous_fr_clean ~ "Cleaning frequency",
                                mud_grass_yard ~ "Mud or grass yard",
                                rodent_sight_fr ~ "Frequency of rodent sight")) %>%
  bold_p() 
# %>% 
#   as_flex_table() %>%
#   save_as_docx(path = "tab3.docx")

# Multivariate model ------------------------------------------------------
fit = glmmTMB(MAT_res ~ street_access + con_garbage_dumps + rodent_sight_fr +
                (1|adm_dis), 
              family = binomial,
              data = dog_df)

### Variable selection
drop1(fit)

fit1 = update(fit, ~.-con_garbage_dumps)
drop1(fit1)

fit2 = update(fit1, ~.-rodent_sight_fr)

# Check model residuals
DHARMa::testResiduals(fit2)

### Coefficients
tbl_regression(fit2, exponentiate = T)

r2(fit2)

# Spatial GAMs ------------------------------------------------------------


