#########################################################################
# Name of file - shes_01_sampling.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Imports the cleaned postcode address file, 
# removes previously sampled addresses, calculates selection probabilities
# and samples the addresses for the Scottish Health Survey (SHeS).

#########################################################################

### 0 - Setup ----

# Run setup script which loads all required packages and 
# functions and executes the config.R script.

source(here::here("code", "00_setup.R"))

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

# Draw stratified systematic sample
# As the selection probability of some addresses is zero,
# a warning message will be displayed when executing the code below.
# This is to be expected and nothing to be concerned about.
shes.totalsample <- shes.sframe %>% 
  sampling(sample_size = shes.samplesize)

# Merge sampling frame and drawn sample and sort data frame
shes.frameandmatchedsample <- shes.sframe %>% 
  merge_frame_sample(shes.totalsample)

### 6 - Export sample  ----

# Code to export sampled addresses into output folder

export_rds(shes.totalsample)

export_rds(shes.samplesize)

export_rds(shes.frameandmatchedsample)
