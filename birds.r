# Load packages
library(dplyr)
library(readr)

# Read in bird checklist
birds <- read_csv("https://raw.githubusercontent.com/KateMMiller/IMD_R_Training_Advanced/main/data/birds.csv")

# Preview dataset
birds

# Verify that all species names are unique
paste("There are", nrow(birds), "rows in this dataset.")
paste("There are", nrow(distinct(birds)), "unique rows in this dataset.")

# How many species groups are there?
paste("There are", n_distinct(birds$species_group), "species groups.")

# Any missing data?
any(is.na(birds$species_group))
any(is.na(birds$primary_com_name))

# Count species in each species group
group_size <- birds %>%
  group_by(species_group) %>%
  summarize(species_count = n()) %>%
  arrange(-species_count) %>%
  ungroup()

# What are the largest species groups?
group_size

# What are the smallest species groups?
tail(group_size)
