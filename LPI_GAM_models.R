# Creating General Additive Models for the LPI, according to Collen et al. (2007)
# John Godlee (johngodlee@gmail.com)

# Packages ----
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(mgcv)
library(broom)

# Set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Data ----
load("LPIdata_Feb2016.RData")
col2[which(!is.finite(col2))] = NA

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
  filter(., lengthyear > 5) %>%  # Exclude columns where dataset <5 years 
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
  # LPI_list <- split(LPI, f = list(LPI$genus_species, LPI$id, LPI$Units))
  # This crashes my laptop
  # Maybe book some time on the big computer
  # LPI_list <- split(LPI, interaction(LPI$genus_species), drop = TRUE)
  # This works but will not make decent models as multiple population estimates
LPI_list <- split(LPI, interaction(LPI$genus_species, LPI$id), drop = TRUE)
  # This works and should not be any worse that including Units as well
    # View(LPI_list[[1]])

# Subset data frame list to list with n>=6 (GAM) and n<6 (chain)
LPI_list_GAM <- LPI_list[sapply(LPI_list, function(df)nrow(df)>=6)] 
LPI_list_chain <-   LPI_list[sapply(LPI_list, function(df)nrow(df)<6)] 

# Create GAMs and LMs for each element in LPI_list_GAM ----
## Create test fits with one data frame in the list
gam_test <- gam(log10(pop)~year, data = LPI_list_GAM[[2]])
lm_test <- lm(log10(pop)~year, data = LPI_list_GAM[[2]])
summary(gam_test)
summary(lm_test)

## For the full list
gam_list <- lapply(LPI_list_GAM, function(x) gam(pop~year, data = x))
lm_list <- lapply(LPI_list_GAM, function(x) lm(pop~year, data = x))

## Checking model fits and output examples 
lm_list[[1]]
typeof(lm_list[[1]])
summary(lm_list[[1]])
glance(lm_list[[1]])
coef(lm_list[[1]])["year"]

gam_list[[1]]
typeof(gam_list[[1]])
summary(gam_list[[1]])
glance(gam_list[[1]])

slope_lm <- gather(as.data.frame(lapply(lm_list, function(x) coef(object = x)["year"])))
slope_lm$value_log <- log(slope_lm$value)
ggplot(slope_lm) + geom_histogram(aes(x = value_log))

# Average GAMs within each species ----

# Plot results of GAM models ----
