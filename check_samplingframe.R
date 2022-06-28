# Preliminary stuff

library(tidyverse)

# Note to self: This isn't working yet since the IDs still have to be cleaned by Christian.

# Test sampling -----------------------------------------------------------

data_kobo <- read_csv("pre_clean_data.csv")

# NON-IDP
data_kobo %>% 
  filter(hh_displacement_status != "yes") %>% 
  group_by(location_id_face) %>% 
  count() %>% 
  view()

# IPD
data_kobo %>%
  filter(hh_displacement_status == "yes") %>% 
  group_by(lga_face) %>% 
  count() %>% 
  view()

samplingframe_idp <- read_csv("samplingframe/Shiny Output IDPs (100%) (10-03-2022).csv") %>% 
  select(-contains("X"))

# clusters per lga

data_kobo %>% 
  #filter(hh_displacement_status == "yes") %>% 
  group_by(lga_face, location_id_face) %>% 
  count() %>% 
  view()

data_kobo %>% 
  group_by(lga_face) %>% 
  distinct(location_id_face, .keep_all = TRUE) %>% 
  count() %>% 
  view()

# batagarawa

data_kobo %>% 
  filter(lga_face == "batagarawa") %>% 
  distinct(location_id_face) %>% 
  pull(location_id_face)
