# reading in kmz files

library(tidyverse)
library(sf)
library(leaflet)

#reading kmz files function

# read_kmz <- function(folder_name, output_rdsfile){
#   
#   kmz_files <- list.files(folder_name, pattern = "csv.kmz")
#   
#   kmz_list <- list()
#   
#   for (kmz_file in kmz_files) {
#     unzipped_kmz <- st_read(unzip(paste0(folder_name, kmz_file)))
#     kmz_list[[kmz_file]] <- unzipped_kmz
#     
#   }
#   
#   kmz_df <- do.call("rbind", kmz_list) %>% 
#     rename(cluster_id = Name) %>% 
#     rownames_to_column("cluster_name") %>% 
#     mutate(cluster_name = gsub(cluster_name, pattern = ".csv.kmz.[0-9]*", replacement = ""),
#            state = gsub(folder_name, pattern = "_kmz/", replacement = "")) %>% 
#     select(-Description)
#   
#   file.remove("doc.kml")
#   
#   saveRDS(kmz_df, file = output_rdsfile)
#   
# }
# 
# read_kmz("katsina_kmz/", output_rdsfile = "katsina_kmzfiles.rds")
# read_kmz("sokoto_kmz/", output_rdsfile = "sokoto_kmzfiles.rds")
# read_kmz("zamfara_kmz/", output_rdsfile = "zamfara_kmzfiles.rds")
# read_kmz("replacements_kmz/", output_rdsfile = "replacements_kmzfiles.rds")

# Validate with actual locations ------------------------------------------

# load kobo data
data_gps <- read_csv("pre_clean_data.csv") %>% 
  drop_na("_gps_longitude", "_gps_latitude") %>%
  st_as_sf(coords = c("_gps_longitude", "_gps_latitude"), crs = 4326)

# load in admin1 boundaries
boundaries_adm1 <- st_read("shapefiles/nga_admbnda_adm1_osgof_20190417.shp") %>% 
  filter(ADM1_EN %in% c("Sokoto", "Katsina", "Zamfara"))
  # select(ADM1_EN, ADM2_EN)

# load in admin2 boundaries
boundaries_adm2 <- st_read("shapefiles/nga_admbnda_adm2_osgof_20190417.shp") %>% 
  filter(ADM1_EN %in% c("Sokoto", "Katsina", "Zamfara"))
# select(ADM1_EN, ADM2_EN)

# load files with assigned interview locations
katsina_shapefile <- read_rds("shapefiles/katsina_kmzfiles.rds")
sokoto_shapefile <- read_rds("shapefiles/sokoto_kmzfiles.rds")
zamfara_shapefile <- read_rds("shapefiles/zamfara_kmzfiles.rds")

replacements_shapefile <- read_rds("shapefiles/replacements_kmzfiles.rds")

katsina_replacements <- replacements_shapefile %>% 
  filter(str_detect(cluster_name, "Katsina"))
sokoto_replacement <- replacements_shapefile %>% 
  filter(str_detect(cluster_name, "Sokoto"))
zamfara_replacement <- replacements_shapefile %>% 
  filter(str_detect(cluster_name, "Zamfara"))

all_kmz <- bind_rows(katsina_shapefile, sokoto_shapefile, zamfara_shapefile, replacements_shapefile)
all_sokoto <- bind_rows(sokoto_shapefile, sokoto_replacement)
all_katsina <- bind_rows(katsina_shapefile, katsina_replacements)
all_zamfara <- bind_rows(zamfara_shapefile, zamfara_replacement)


#map
# ggplot() +
#   geom_sf(data = boundaries_adm1) +
#   geom_sf(data = all_kmz) +
#   geom_sf(data = data_gps, color = "red", alpha = 0.25) +
#   labs(title = "Sampled (black) and actual (red) interview locations",
#        subtitle = "States: Zamfara, Katsina, Sokoto") +
#   theme_minimal()

#overall map
ggplot() +
  geom_sf(data = boundaries_adm2, fill = "grey", color = "white", ) +
  geom_sf(data = boundaries_adm1, fill = NA, color = "black", ) +
  geom_sf(data = all_kmz, alpha = 0.5) +
  geom_sf(data = data_gps, aes(color = organisation_of_enumerator), alpha = 0.25) +
  geom_sf_label(data = boundaries_adm1, aes(label = ADM1_EN)) +
  labs(title = "Sampled (black) and actual interview locations coloured by organisation",
       subtitle = "States: Zamfara, Katsina, Sokoto",
       color = "Enumerator Organisation",
       y = "",
       x = "") +
  theme_minimal()

# zamfara map
ggplot() +
  geom_sf(data = boundaries_adm2, fill = "grey", color = "white", ) +
  geom_sf(data = boundaries_adm1, fill = NA, color = "black", ) +
  geom_sf(data = all_zamfara, alpha = 0.5) +
  geom_sf(data = subset(data_gps, state_face == "zamfara"), aes(color = organisation_of_enumerator), alpha = 0.25) +
  geom_sf_label(data = boundaries_adm1, aes(label = ADM1_EN)) +
  labs(title = "Sampled (black) and actual interview locations coloured by organisation",
       subtitle = "States: Zamfara, Katsina, Sokoto",
       color = "Enumerator Organisation",
       y = "",
       x = "") +
  theme_minimal()

subset(data_gps, state_face == "zamfara") %>% view


# See how many exactly overlap --------------------------------------------

interviews_sokoto <- data_gps %>% 
  filter(state_face == "sokoto")

for (threshold in seq(250, 2000, 250)) {
  
  boundary_list <- st_is_within_distance(x = interviews_sokoto, y = all_sokoto, dist = threshold)
  share_within <- sum(lengths(boundary_list) > 0)/nrow(interviews_sokoto)
  print(paste("State: Sokoto", "Share:", round(share_within, digits = 2), "Threshold:", threshold))
  
}

overlap_interviews <- function(kobodata, name_of_state, shape_file_state, threshold_points = seq(250, 2000, 250)){
  
  interviews <- kobodata %>% 
    filter(state_face == name_of_state)
  
  for (threshold in threshold_points) {
    
    boundary_list <- st_is_within_distance(x = interviews, y = shape_file_state, dist = threshold)
    share_within <- sum(lengths(boundary_list) > 0)/nrow(interviews)
    print(paste("State:", name_of_state, "Share:", round(share_within, digits = 2), "Threshold:", threshold))
    
  }
  
}

overlap_interviews(data_gps, "sokoto", all_sokoto, threshold = 1000)
overlap_interviews(data_gps, "kastina", all_katsina, threshold = 1000)


# binary creator
binary_vect <- rep(NA, length(sokoto_list))

for (i in 1:length(sokoto_list)) {
  
  if (is_empty(sokoto_list[[i]])) {
    binary_vect[i] <- 1
  } else {
    binary_vect[i] <- 0
  }
    
  
}

sokoto_augemented <- interviews_sokoto %>% 
  mutate(empty = as.factor(binary_vect))

ggplot(data = sokoto_augemented) + 
  geom_sf(color = "black") +
  geom_sf(aes(color = empty), alpha = 0.1) 

# empty ones

empty_sokoto <- sokoto_augemented %>% 
  filter(empty == 1)

ggplot() +
  geom_sf(data = subset(empty_sokoto, lga_face == "yabo"), color = "red") +
  geom_sf(data = subset(sokoto_shapefile, cluster_name == "Main Sokoto Yabo"), color = "blue")


# Sokoto ------------------------------------------------------------------


# yabo
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = subset(interviews_sokoto, lga_face == "yabo"), color = "blue") %>% 
  addCircleMarkers(data = subset(sokoto_shapefile, cluster_name == "Main Sokoto Yabo"), color = "red") %>% 
  addCircleMarkers(data = subset(replacements_shapefile, cluster_name == "Replacement Sokoto Yabo"), color = "yellow") %>%  
  addMeasure(primaryLengthUnit = "kilometers")

yabo <- data_gps %>% 
  filter(lga_face == "yabo")

overlap_interviews(yabo, name_of_state = "sokoto", shape_file_state = sokoto_shapefile, threshold_points = 300)
overlap_interviews(yabo, name_of_state = "sokoto", shape_file_state = all_sokoto, threshold_points = 300)

# tambuwal
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = subset(interviews_sokoto, lga_face == "tambuwal"), color = "blue") %>% 
  addCircleMarkers(data = subset(sokoto_shapefile, cluster_name == "Main Sokoto Tambuwal"), color = "red") %>% 
  addCircleMarkers(data = subset(replacements_shapefile, cluster_name == "Replacement Sokoto Tambuwal"), color = "yellow") %>%  
  addMeasure(primaryLengthUnit = "kilometers")

tambuwal <- data_gps %>% 
  filter(lga_face == "tambuwal")

overlap_interviews(tambuwal, name_of_state = "sokoto", shape_file_state = sokoto_shapefile, threshold_points = 300)
overlap_interviews(tambuwal, name_of_state = "sokoto", shape_file_state = all_sokoto, threshold_points = 300)

#illela
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = subset(interviews_sokoto, lga_face == "illela"), color = "blue") %>% 
  addCircleMarkers(data = subset(sokoto_shapefile, cluster_name == "Sokoto Illela"), color = "red")

#gwadabawa
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = subset(interviews_sokoto, lga_face == "gwadabawa"), color = "blue") %>% 
  addCircleMarkers(data = subset(sokoto_shapefile, cluster_name == "Sokoto Gwadabawa"), color = "red") 
  addMeasure(primaryLengthUnit = "kilometers") 


# Interview rate ----------------------------------------------------------

library(lubridate)

data_gps %>% 
  st_drop_geometry() %>% 
  group_by(today) %>% 
  count(name = "interviews") %>% 
  ungroup() %>% 
  mutate(avg_interviews = mean(interviews)) %>% 
  ggplot(aes(x = today, y = interviews)) +
  scale_x_date(date_labels="%D",date_breaks  ="1 day") +
  geom_hline(aes(yintercept = avg_interviews), linetype = "dashed", color = "red") +
  geom_col() +
  labs(title = "On average, the teams conduct 100 interviews per day", 
       subtitle = "With that rate, we need roughly another two weeks",
       y = "Number of interviews\n",
       x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
