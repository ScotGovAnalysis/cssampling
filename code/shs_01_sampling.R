#########################################################################
# Name of file - shs_01_sampling.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Imports the cleaned postcode address file, 
# removes previously sampled addresses, calculates selection probabilities
# and samples the addresses for the Scottish Household Survey (SHS).

#########################################################################

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

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
shs.samplesize <- read.csv(shs.samplesize.path, 
                                    header = TRUE, na = "") %>%
  clean_names_modified()

### 2 - Used addresses ---- 

shs.sframe <- used_addresses(prev_samples = usedaddresses,
               paf = clean_paf)
nrow(shs.sframe)

### 3 - Multiple occupancy  ---- 

shs.sframe <- shs.sframe %>% multiple_occupancy()
nrow(shs.sframe)

### 4 - Sampling ---- 

# Draw stratified systematic sample
shs.totalsample <- shs.sframe %>%
  sampling(sample_size = shs.samplesize)

# Merge sampling frame and drawn sample and sort data frame
shs.frameandmatchedsample <- shs.sframe %>% 
  merge_frame_sample(shs.totalsample)

### 5 - Export sample  ----

# Code to export sampled addresses into output folder

export_rds(shs.totalsample)

export_rds(shs.samplesize)

export_rds(shs.frameandmatchedsample)
