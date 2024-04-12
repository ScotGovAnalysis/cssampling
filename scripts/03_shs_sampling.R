#########################################################################
# Name of file - 03_shs_sampling.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Imports the cleaned postcode address file, 
# removes previously sampled addresses, calculates selection probabilities
# and samples the addresses for the Scottish Household Survey (SHS).

#########################################################################

# clear environment
rm(list=ls())

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("scripts", "00_setup.R"))

### 1 - Import files ---- 

# Identify most recent used addresses file
recent_usedaddresses <- most_recent_file(path = here("lookups"), 
                                         pattern = "usedaddresses")

# Import used addresses
usedaddresses <- read_rds(paste0(here("lookups"), "/", recent_usedaddresses))

# Identify most recent cleaned PAF
recent_paf <- paf_list[grepl(paf_v, paf_list, 
                             ignore.case = TRUE)]

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

shs.control <- c("dz11_urbrur2020",
                  "simd20rank",
                  "postcode",
                  "print_address")

# Draw stratified systematic sample
# As the selection probability of some addresses is zero,
# a warning message will be displayed when executing the code below.
# This is to be expected and nothing to be concerned about.
shs.totalsample <- shs.sframe %>%
  sampling(stratum = "la_code",
           sample_size = shs.samplesize$total_n,
           prob = "totalsize",
           control = shs.control)

# Merge sampling frame and drawn sample and sort data frame
shs.frameandmatchedsample <- shs.sframe %>% 
  merge_frame_sample(shs.totalsample)

### 5 - Draw reserve sample ---- 

# Draw stratified systematic sample
# As the selection probability of some addresses is zero,
# a warning message will be displayed when executing the code below.
# This is to be expected and nothing to be concerned about.
shs.reservesample <- shs.totalsample %>% 
  sampling(stratum = "la_code",
           sample_size = shs.samplesize$reserve_n,
           prob = rep(1/nrow(shs.totalsample), 
                      times = nrow(shs.totalsample)),
           control = shs.control)

# Remove reserve sample from contractor sample
shs.mainsample <- anti_join(x = shs.totalsample, 
                            y = shs.reservesample,
                            by = "udprn")
nrow(shs.mainsample)

### 6 - Household condition ---- 

shs.contractorsample <- shs.mainsample %>%
  
  # Draw house condition sample
  sampling(stratum = "la_code",
           sample_size = shs.samplesize$house_condition_n,
           prob = rep(1/nrow(shs.mainsample), 
                      times = nrow(shs.mainsample)),
           control = shs.control) %>%
  
  # Add flag for sampled addresses
  mutate(houseconditionflag = 1) %>%
  
  # Merge with main sample
  right_join(shs.mainsample) %>%
  
  # Replace NAs in houseconditionflag with 0
  mutate(houseconditionflag = replace_na(houseconditionflag, 0))

nrow(shs.contractorsample)

### 7 - Prepare for export ----

shs.contractor.export <- shs.contractorsample %>% prepare_for_export()

### 8 - Post-processing ---- 

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

### 9 - Export sample  ----

# Code to export sampled addresses into output folder
# Current date is automatically added to file name to avoid 
# overwriting existing files

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

### END OF SCRIPT ####

# clear environment
rm(list=ls())
