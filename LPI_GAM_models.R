# Creating General Additive Models for the LPI, according to Collen et al. (2007)
# John Godlee (johngodlee@gmail.com)

# Packages ----
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(mgcv)

# Set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Data ----
load("LPIdata_Feb2016.RData")

# Convert data to long format and add useful columns ----
LPI <- LPIdata_Feb2016 %>%
  gather("year", "pop", 26:70)  %>%  # Transform to long format, gathering each 
  mutate(year = parse_number(.$year)) %>%  # Deprecated, extract_numeric() -> parse_numeric() -> parse_number(), extract numeric from atomic
  mutate(., genus_species = paste(Genus, Species, sep = '_')) %>%  # Create a species column by concatenating genus and species
  filter(., is.finite(pop)) %>%  # Keep only rows with a population estimate
  distinct(.) %>%  # Remove duplicate rows
  group_by(., genus_species, id) %>%  # group rows so that each group is one population (id) from one Species(genus_species)
  mutate(., maxyear = max(year), minyear = min(year)) %>%  # Create a column for the max and min years for each group
  mutate(., lengthyear = maxyear-minyear) %>%  # Create a column for the length of time data available
  ungroup(.) %>%  # Remove groupings
  group_by(., genus_species, id, Units) %>%  # Groups Measurement_type(Units)>population(id)>Species(genus_species)
  mutate(., scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>%  # Scale population trend from 0 to 1
  filter(., is.finite(scalepop)) %>%  # Remove rows without a scalepop
  mutate(., meanpop = mean(pop)) %>%  # Create column for mean population
  ungroup(.) %>%
  group_by(., genus_species, id) %>%
  mutate(., meanpop.size = mean(meanpop)) %>%  # Create column for mean mean population
  ungroup(.)

# Split the data frame into a list of data frames based on species, collection id, and units of collection ----
LPI_list <- split(LPI, f = list(LPI$genus_species, LPI$id, LPI$Units))
  # This crashes my laptop
  # Maybe book some time on the big computer

# Create GAM models for each list item using mgcv ----

# Average GAMs within each species ----

# Plot results of GAM models ----
