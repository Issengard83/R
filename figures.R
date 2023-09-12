# LOAD PACKAGES -----------------------------------------------------------
pacman::p_load(
  # Map tools
  sf, 
  OpenStreetMap,
  ggimage,
  ggrepel,
  # Colorblind-friendly palettes
  scico,
  # Data management
  tidyverse,
  janitor)

# Load socioeconomic indicators layer -------------------------------------
var_ct = st_read("SHP/VAR_CT_SF.shp")%>% 
  # Raw indicators as percentages
  mutate_at(vars(starts_with("h_")|matches("pc_hog")), 
            list(pct = ~ round(.*100/n_hogares,2))) %>% 
  # Remove census tracts outside the city area
  filter(redcode!="820630317")

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

# Create city districts layer ---------------------------------------------
adm_dis = var_ct %>% group_by(adm_dis) %>% 
  summarise(n = n()) %>% 
  # Get coordinates
    mutate(lon = st_coordinates(st_centroid(.))[,1],
           lat = st_coordinates(st_centroid(.))[,2]) %>%
  st_simplify(dTolerance = .3)

# Create base map ---------------------------------------------------------
base_map = openmap(upperLeft = c(max(var_ct$lat) + .03, max(var_ct$lon) + .03),
              lowerRight = c(min(var_ct$lat) - .03, min(var_ct$lon) - .03),
              type = "osm") %>% 
  # Projection WGS84
  openproj(projection = "epsg:4326")

# Load IMUSA markers ------------------------------------------------------
imu_loc = st_read("SHP/IMUSA_SF.shp") %>% 
  mutate(image = if_else(tipo=="móvil", 
                         "SHP/van_10551021.png",
                         "SHP/location_10551218.png"),
         size = if_else(tipo=="móvil", .035, .05))

# Figure 1 ----------------------------------------------------------------
autoplot.OpenStreetMap(base_map) + 
  # Unsatisfied basic needs by census tract
  geom_sf(data = var_ct, aes(x = lon, y = lat, fill = h_nbi_pct), color = NA) +
  scale_fill_scico(palette = "turku", direction = -1,
                   name = "NBI (%)") +
  # Informal settlements
  geom_sf(data = inf_st, aes(x = lon, y = lat, color = cat), alpha = .4, lwd = .7) +
  scale_color_scico_d(palette = "buda", name = "") +
   # City districts
  geom_sf(data = adm_dis, aes(x = lon, y = lat), fill = NA) +
  geom_label_repel(data = adm_dis, aes(x = lon, y = lat, label = adm_dis),
                   alpha = .6, max.overlaps = 2, size = 3, label.size = .2,
                   min.segment.length = .4, nudge_y = .004, nudge_x = -.002) +
  # IMUSA locations
  geom_image(data = imu_loc, aes(x = lon, y = lat, image = image, size = I(size)),
             position = position_jitter(width = 0, height = .015, seed = 10)) +
  # Set theme
  theme_minimal() + theme(axis.title = element_blank(), legend.position = "bottom")

### Save image
ggsave("FIGS/fig1.png", width = 15, height = 15, units = "cm", dpi = 300)
