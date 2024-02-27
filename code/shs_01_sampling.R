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
# As the selection probability of some addresses is zero,
# a warning message will be displayed when executing the code below.
# This is to be expected and nothing to be concerned about.
shs.totalsample <- shs.sframe %>%
  sampling(sample_size = shs.samplesize$total_n)

# Merge sampling frame and drawn sample and sort data frame
shs.frameandmatchedsample <- shs.sframe %>% 
  merge_frame_sample(shs.totalsample)

### 5 - Draw reserve sample ---- 

# Draw stratified systematic sample
# As the selection probability of some addresses is zero,
# a warning message will be displayed when executing the code below.
# This is to be expected and nothing to be concerned about.
shs.reservesample <- shs.totalsample %>% 
  sampling(sample_size = shs.samplesize$reserve_n)

# Remove reserve sample from contractor sample
shs.mainsample <- anti_join(x = shs.totalsample, 
                            y = shs.reservesample,
                            by = "udprn")
nrow(shs.mainsample)

### 6 - Household condition ---- 

shs.contractorsample <- shs.mainsample %>%
  
  # Draw house condition sample
  sampling(sample_size = shs.samplesize$house_condition_n) %>%
  
  # Add flag for sampled addresses
  mutate(houseconditionflag = 1) %>%
  
  # Merge with main sample
  right_join(shs.mainsample) %>%
  
  # Replace NAs in houseconditionflag with 0
  mutate(houseconditionflag = replace_na(houseconditionflag, 0))

nrow(shs.contractorsample)

### 7 - Prepare for export ----

shs.contractor.export <- shs.contractorsample %>% prepare_for_export()

### 7 - Post-processing ---- 

# generate streams 1:4
streams1 <- stream_allocation(1, 4)

# generate streams 5:12
streams0 <- stream_allocation(5, 12)

# add streams to contractor sample
shs.contractor.export <- shs.contractor.export %>%
  
  # if an address is part of the house condition sample,
  # assign streams 1:4
  mutate(stream = ifelse(houseconditionflag == 1,
                         rep_len(streams1$num, 
                                 length.out = nrow(.)),
                         0),
         
         # if an address is not part of the house condition sample,
         # assign streams 5:12
         stream = ifelse(houseconditionflag == 0,
                         rep_len(streams0$num, 
                                 length.out = nrow(.)),
                         stream))

# Determine which households at multiple occupancy addresses gets interviewed
shs.contractor.export <- shs.contractor.export %>% selected_mo()

### 8 - Export sample  ----

# Code to export sampled addresses into output folder

export_rds(shs.totalsample)

export_rds(shs.frameandmatchedsample)

export_rds(shs.contractorsample)

export_rds(shs.reservesample)

write.csv(shs.contractor.export, 
          paste0(shs.path,
                 "shs.contractorsample.",
                 syear,
                 ".csv"),
          row.names = FALSE)
