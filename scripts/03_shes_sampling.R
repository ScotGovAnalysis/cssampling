#########################################################################
# Name of file - 03_shes_sampling.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Imports the cleaned postcode address file, 
# removes previously sampled addresses, calculates selection probabilities
# and samples the addresses for the Scottish Health Survey (SHeS).

#########################################################################

# clear environment
rm(list=ls())

### 0 - Setup ----

# Run setup script which loads all required packages and 
# functions and executes the config.R script.

source(here::here("scripts", "00_setup.R"))

### 1 - Import files ---- 

# Identify most recent used addresses file
recent_usedaddresses <- most_recent_file(path = here("lookups"), 
                                         pattern = "usedaddresses")

# Import used addresses
usedaddresses <- read_rds(paste0(here("lookups"), "/", recent_usedaddresses))

# Identify most recent cleaned PAF
recent_paf <- most_recent_file(path = here("lookups"), pattern = "paf")

# Import cleaned PAF
clean_paf <- read_rds(paste0(here("lookups", "/", recent_paf)))

# Import SIMD ranks for datazones
dz11_simd20 <- haven::read_sas(dz_simd.path) %>%
  clean_names_modified()

# Import sample size file
shes.samplesize <- read.csv(shes.samplesize.path, 
                                   header = TRUE, na = "") %>%
  clean_names_modified()

# Import SHeS cluster file
shes_clusters <- read.csv(shes.clusters.path, 
                                    header = TRUE, na = "") %>%
  clean_names_modified()

### 2 - Used addresses ---- 

shes.sframe <- used_addresses(prev_samples = usedaddresses,
                             paf = clean_paf)
nrow(shes.sframe)

### 3 - Cluster size ---- 

shes.sframe <- shes.sframe %>% add_clusters(clusters = shes_clusters)
nrow(shes.sframe)

### 4 - Multiple occupancy  ---- 

shes.sframe <- shes.sframe %>% multiple_occupancy()
nrow(shes.sframe)

### 5 - Sampling ---- 

shes.control <- c("dz11_urbrur2020",
                  "average_simd20_rank",
                  "simd20rank",
                  "postcode",
                  "print_address")

# Draw stratified systematic sample
shes.totalsample <- shes.sframe %>%
  sampling(stratum = "la_code",
           sample_size = shes.samplesize$total_n,
           prob = "totalsize",
           control = shes.control)

# Merge sampling frame and drawn sample and sort data frame
shes.frameandmatchedsample <- shes.sframe %>% 
  merge_frame_sample(shes.totalsample)

### 6 - Export sample  ----

# Code to export sampled addresses into output folder
# Current date is automatically added to file name to avoid 
# overwriting existing files

export_rds(shes.totalsample)

export_rds(shes.samplesize)

export_rds(shes.frameandmatchedsample)

### END OF SCRIPT ####

# clear environment
rm(list=ls())
