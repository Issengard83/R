### Figures and maps generated for the manuscript: 
### Seroprevalence of Leptospira antibodies in dogs and cats attending to 
### municipal spay/neuter campaigns, Santa Fe, Argentina.
### Author: Tamara Ricardo
### Last update: 2023-09-14

# LOAD PACKAGES -----------------------------------------------------------
pacman::p_load(
  # Map tools
  sf, 
  OpenStreetMap,
  ggimage,
  # Colorblind-friendly palettes
  scico,
  # Data management
  tidyverse,
  janitor)

# Load socioeconomic indicators layer -------------------------------------
var_ct = st_read("SHP/VAR_CT_SF.shp") %>% 
  # # Raw indicators as percentages
  # mutate_at(vars(starts_with("h_")|matches("pc_hog")), 
  #           list(pct = ~ round(.*100/n_hogares,2))) %>% 
  ### Reorder factor levels
  mutate(pc_incid_c = pc_incid_c %>% as_factor %>% 
           fct_relevel("High (10-15%)", after = 2))
  

# Load informal settlements layer -----------------------------------------
inf_st = st_read("SHP/raw/RENABAP_SF.shp", stringsAsFactors = T) %>% 
  # Clean variable names
  clean_names() %>% 
  rename(lat = latitud_ce, lon = longitud_c) %>% 
  # Drop irrelevant variables
  select(nombre_bar, clasificac, cloacas, agua, lat, lon) %>% 
  # Remove settlements outside the city area
  filter(nombre_bar!="La Balsa") %>% 
  # Recode levels of classification
  mutate(cat = if_else(clasificac=="Villa", "Slum", "Settlement"))


# Load administrative districts layer -------------------------------------
adm_dis = st_read("SHP/ADM_DIS_SF.shp")

# Load IMUSA markers ------------------------------------------------------
imu_loc = st_read("SHP/IMUSA_SF.shp") %>% 
  mutate(image = if_else(tipo=="móvil", 
                         "SHP/van_10551021.png",
                         "SHP/location_10551218.png"),
         size = if_else(tipo=="móvil", .035, .05))


# Create base map ---------------------------------------------------------
base_map = openmap(upperLeft = c(max(var_ct$lat) + .03, max(var_ct$lon) + .03),
                   lowerRight = c(min(var_ct$lat) - .03, min(var_ct$lon) - .03),
                   type = "osm") %>% 
  # Projection WGS84
  openproj(projection = "epsg:4326") %>% 
  autoplot.OpenStreetMap() +
  # Set theme
  theme_minimal() + theme(axis.title = element_blank(), legend.position = "bottom")

# Figure 1 ----------------------------------------------------------------
base_map + 
  ## Chronic poverty by census tract
  geom_sf(data = var_ct, aes(x = lon, y = lat, fill = pc_incid_c), color = NA) +
  scale_fill_scico_d(palette = "tokyo", direction = 1,
                   name = "") +
  ## Informal settlements
  geom_sf(data = inf_st, aes(x = lon, y = lat), alpha = .25, lwd = .5, color = "#B301B3") +
  ## City districts
  geom_sf(data = adm_dis, aes(x = lon, y = lat), fill = "NA") +
  geom_sf_label(data = adm_dis, aes(x = lon, y = lat, label = adm_dis), 
                alpha = .5, size = 3) +
  # geom_label_repel(data = adm_dis, aes(x = lon, y = lat, label = adm_dis),
  #                  alpha = .6, max.overlaps = 2, size = 3, label.size = .2,
  #                  min.segment.length = .4, nudge_y = .004, nudge_x = -.002) +
  ## IMUSA locations
  geom_image(data = imu_loc, aes(x = lon, y = lat, image = image, size = I(size)),
             position = position_jitter(width = 0, height = .015, seed = 10))

### Save image
ggsave("FIGS/fig1.png", width = 16, height = 16, units = "cm", dpi = 300)

# Figure 2 ----------------------------------------------------------------
