library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

kits_data <- read.csv("DATA COLLECTION - Cleaned Kits Values.csv")
renfrew_data <- read.csv("DATA COLLECTION - Cleaned Renfrew Values.csv")

public_trees <- read.csv2("public-trees.csv") %>%
  rename(tree_id = TREE_ID)

kits_total_dataset <- kits_data %>%
  left_join(public_trees, by = "tree_id") %>% 
  mutate(Cover_Type = "High Canopy Cover")

renfrew_total_dataset <- renfrew_data %>%
  left_join(public_trees, by = "tree_id") %>% 
  mutate(Cover_Type = "Low Canopy Cover")

all_trees_total <- rbind(renfrew_total_dataset, kits_total_dataset) %>%
  rename(hig_bryo = hig_byro) %>%
  mutate(total_abundance = low_bryo + med_bryo + hig_bryo + 
           low_lichen + med_lichen + hig_lichen) %>%
  mutate(total_species = low_species + med_species + hig_species) %>%
  mutate(HBLOCK = str_to_upper(trimws(paste(ON_STREET_BLOCK, ON_STREET)))) %>%
  separate(geo_point_2d, into = c("lat", "lon"), sep = ", ") %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon))

public_streets <- read.csv2("public-streets.csv") %>%
  mutate(
    start_num = str_extract(HBLOCK, "^[0-9]+"),
    end_num = str_extract(HBLOCK, "(?<=-)[0-9]+")
  ) %>%
  pivot_longer(cols = c(start_num, end_num), values_to = "BLOCK_NUM") %>%
  filter(!is.na(BLOCK_NUM)) %>%
  mutate(
    STREET_NAME = str_remove(HBLOCK, "^[0-9-]+ "),
    HBLOCK_CLEAN = paste(BLOCK_NUM, STREET_NAME)
  ) %>%
  distinct(HBLOCK_CLEAN, .keep_all = TRUE)

all_trees_total <- all_trees_total %>%
  select(-any_of("STREETUSE")) %>%
  mutate(HBLOCK = str_to_upper(trimws(HBLOCK))) %>%
  # Join to get the categories
  left_join(
    public_streets %>% select(HBLOCK_CLEAN, STREETUSE), 
    by = c("HBLOCK" = "HBLOCK_CLEAN")
  ) %>%
  # IMPORTANT: Apply manual fixes AFTER the join so they aren't overwritten
  mutate(STREETUSE = case_when(
    HBLOCK %in% c("2600 NOOTKA ST", "5200 RHODES ST") ~ "Residential",
    # Add any other manual arterial/residential assignments here
    TRUE ~ STREETUSE # Keeps the values from the join if they aren't in the manual list
  ))
