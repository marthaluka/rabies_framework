# ============================================================
# Global map of rabies modelling settings
# ============================================================

require(pacman)

pacman::p_load(
  sf, tidyverse,
  rnaturalearth,
  geodata, terra
)


# Directory for downloaded GADM files
dir.create("data/gadm", recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# 1. Global country boundaries
# ------------------------------------------------------------

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

study_country_codes <- c("IND", "KEN", "MDG", "NGA", "PHL", "TZA")

world <- world %>%
  mutate(study_country = if_else(adm0_a3 %in% study_country_codes, "Study country", "Other country"))



# ------------------------------------------------------------
# 2. Download administrative boundaries
# ------------------------------------------------------------

# ADM1 is sufficient for:
# Kerala, Kaduna, MIMAROPA, Mara, Lindi, Mtwara and Zanzibar
#
# Moramanga is a district, so we need ADM2 for Madagascar.

india_adm1 <- geodata::gadm(
  country = "IND",
  level = 1,
  path = "data/gadm"
) %>%
  st_as_sf()

madagascar_adm3 <- geodata::gadm(
  country = "MDG",
  level = 3,
  path = "data/gadm"
) %>%
  st_as_sf()

sort(unique(madagascar_adm3$NAME_3))

nigeria_adm1 <- geodata::gadm(
  country = "NGA",
  level = 1,
  path = "data/gadm"
) %>%
  st_as_sf()

philippines_adm1 <- geodata::gadm(
  country = "PHL",
  level = 1,
  path = "data/gadm"
) %>%
  st_as_sf()

mimaropa_provinces <- c(
  "Occidental Mindoro",
  "Oriental Mindoro",
  "Marinduque",
  "Romblon",
  "Palawan"
)

mimaropa <- philippines_adm1 %>%
  filter(NAME_1 %in% mimaropa_provinces) %>%
  summarise(
    geometry = st_union(geometry)
  ) %>%
  mutate(
    setting = "MIMAROPA Region, Philippines"
  ) %>%
  select(setting, geometry)


tanzania_adm1 <- geodata::gadm(
  country = "TZA",
  level = 1,
  path = "data/gadm"
) %>%
  st_as_sf()

kerala <- india_adm1 %>%
  filter(NAME_1 == "Kerala") %>%
  transmute(
    setting = "Kerala State, India",
    geometry
  )

moramanga <- madagascar_adm3 %>%
  filter(NAME_3 == "Moramanga") %>%
  transmute(
    setting = "Moramanga District, Madagascar",
    geometry
  )

kaduna <- nigeria_adm1 %>%
  filter(NAME_1 == "Kaduna") %>%
  transmute(
    setting = "Kaduna State, Nigeria",
    geometry
  )

# Lindi + Mtwara are treated as one model setting
lindi_mtwara <- tanzania_adm1 %>%
  filter(NAME_1 %in% c("Lindi", "Mtwara")) %>%
  summarise(
    geometry = st_union(geometry)
  ) %>%
  mutate(
    setting = "Lindi-Mtwara Regions, Tanzania"
  ) %>%
  select(setting, geometry)


# Mara
mara <- tanzania_adm1 %>%
  filter(NAME_1 == "Mara") %>%
  transmute(
    setting = "Mara Region, Tanzania",
    geometry
  )

shinyanga <- tanzania_adm1 %>%
  filter(NAME_1 == "Shinyanga") %>%
  transmute(setting = "Shinyanga Region, Tanzania", geometry)

sort(unique(tanzania_adm1$NAME_1))

zanzibar_regions <- c(
  "Kaskazini Pemba",
  "Kusini Pemba",
  "Kaskazini Unguja",
  "Kusini Unguja",
  "Mjini Magharibi"
)

zanzibar <- tanzania_adm1 %>%
  filter(NAME_1 %in% zanzibar_regions) %>%
  summarise(
    geometry = st_union(geometry)
  ) %>%
  mutate(
    setting = "Zanzibar, Tanzania"
  ) %>%
  select(setting, geometry)

# Combine all subnational model settings
subnational_settings <- bind_rows(
  kerala,
  moramanga,
  kaduna,
  mimaropa,
  lindi_mtwara,
  mara,
  shinyanga,
  zanzibar
)




# plot 
# ------------------------------------------------------------
# 6. Global map
# ------------------------------------------------------------

# 1. Add short labels
subnational_settings <- subnational_settings %>%
  mutate(label = case_when(
    setting == "Kerala State, India" ~ "Kerala",
    setting == "Moramanga District, Madagascar" ~ "Moramanga",
    setting == "Kaduna State, Nigeria" ~ "Kaduna",
    setting == "MIMAROPA Region, Philippines" ~ "MIMAROPA",
    setting == "Lindi-Mtwara Regions, Tanzania" ~ "Lindi–Mtwara",
    setting == "Mara Region, Tanzania" ~ "Mara",
    setting == "Shinyanga Region, Tanzania" ~ "Shinyanga",
    setting == "Zanzibar, Tanzania" ~ "Zanzibar"
  ))


national_settings <- world %>%
  filter(adm0_a3 %in% c("KEN", "NGA", "TZA")) %>%
  transmute(
    setting = recode(
      adm0_a3,
      KEN = "Kenya, national",
      NGA = "Nigeria, national",
      TZA = "Tanzania, national"
    ),
    geometry
  ) %>%
  mutate(label = recode(
    setting,
    "Kenya, national" = "Kenya",
    "Nigeria, national" = "Nigeria",
    "Tanzania, national" = "Tanzania"
  ))


national_settings

# 2. Calculate positions where labels should originate
map_labels <- bind_rows(
  subnational_settings %>% select(setting, label, geometry),
  national_settings %>% select(setting, label, geometry)
) %>%
  st_point_on_surface() %>%
  mutate(x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2])

# 3. Plot
p_map <- ggplot() +
  geom_sf(data = world, fill = "grey77", colour = "white", linewidth = 0.1) +
  geom_sf(data = national_settings, aes(fill = "National setting"), colour = "white", linewidth = 0.2) +
  geom_sf(data = subnational_settings, aes(fill = "Subnational setting"), colour = "black", linewidth = 0.2) +
  geom_text_repel(data = map_labels, aes(x = x, y = y, label = label), size = 3.3, min.segment.length = 0, seed = 143) +
  scale_fill_manual(name = NULL, values = c("National setting" = "#4C78A8", "Subnational setting" = "#E6863B")) +
  coord_sf(xlim = c(-30, 175), ylim = c(-50, 80), expand = FALSE) +
  theme_void(base_size = 11) +
  theme(panel.background = element_rect(fill = "#C1F2FE", colour = NA),
        legend.position = "bottom")

#p_map


pdf("output/Fig2.pdf", width = 9,height = 6)
p_map
dev.off()



