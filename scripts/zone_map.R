library(tidyverse)
library(sf)
library(janitor)

shapefile <- st_read("data/Zoning.shp")

glimpse(shapefile)

df <- shapefile %>% 
  mutate(residential = str_detect(full_zonin, "RESIDENT"),
         single_unit = str_detect(full_zonin, "SINGLE-UNIT"),
         attached = str_detect(full_zonin, "ATTACHED"),
         type = case_when(residential == TRUE & single_unit == TRUE & attached == FALSE ~ "Single-unit detached residential",
                          residential == TRUE & single_unit == FALSE | attached == TRUE ~ "Other residential",
                          full_zonin == "EDUCATIONAL/MEDICAL INSTITUTION" ~ "Educational/Medical",
                          residential == FALSE ~ "Other non-residential"),
         type = factor(type, levels = c("Single-unit detached residential", 
                                        "Other residential",
                                        "Educational/Medical",
                                        "Other non-residential")),
         alpha_flag = type == "Single-unit detached residential")

test <- df %>% 
  count(full_zonin, type, sort = TRUE) %>% 
  st_drop_geometry()
test

pct_single <- df %>% 
  st_drop_geometry() %>% 
  filter(residential == TRUE) %>% 
  summarize(pct_single_unit_detached_residential = mean(type == "Single-unit detached residential")) %>% 
  mutate(pct_single_unit_detached_residential = round(pct_single_unit_detached_residential, digits = 3)) %>% 
  pull(pct_single_unit_detached_residential)

zone_map <- df %>% 
  ggplot(aes(fill = type)) +
    geom_sf() +
    scale_fill_manual("Zone type",
                      values = c("green", "blue", "yellow", "light grey")) +
    labs(title = "34% of residential zoned land area is single-family detached",
         subtitle = "CIty of Pittsburgh zoning",
         caption = "@conor_tompkins, data from WPRDC") +
    theme_void()
zone_map

ggsave("output/zone_map.png", zone_map)


  