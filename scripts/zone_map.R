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
                          full_zonin == "EDUCATIONAL/MEDICAL INSTITUTION" ~ "Edu/Medical",
                          residential == FALSE ~ "Other non-residential"),
         alpha_flag = type == "Single-unit detached residential")

test <- df %>% 
  count(full_zonin, type, sort = TRUE) %>% 
  st_drop_geometry()
test

df %>% 
  ggplot(aes(fill = type, alpha = alpha_flag)) +
  geom_sf(color = NA) +
  scale_alpha_discrete(range = c(.3, 1))+
  guides(alpha = FALSE) +
  theme_void()

df %>% 
  st_drop_geometry() %>% 
  filter(residential == TRUE) %>% 
  summarize(pct_single_unit_detached_residential = mean(type == "Single-unit detached residential"))
  