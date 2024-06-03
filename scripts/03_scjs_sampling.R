#########################################################################
# Name of file - 03_scjs_sampling.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Imports the cleaned postcode address file, 
# removes previously sampled addresses, calculates selection probabilities
# and samples the addresses for the Scottish Crime and Justice Survey (SCJS).

#########################################################################

# clear environment
rm(list=ls())

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message(title("\nExecute sampling script"))

### 1 - Import files ---- 

# Add message to inform user about progress
message(normal("Import data"))

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

# Import sample size file
scjs.samplesize <- read.csv(scjs.samplesize.path, 
                                    header = TRUE, na = "") %>%
  clean_names_modified()

# Import SIMD ranks for datazones
dz11_simd20 <- haven::read_sas(dz_simd.path) %>%
  clean_names_modified()

### 2 - Used addresses ---- 

# Add message to inform user about progress
message(normal("Add metrics for active addresses"))

scjs.sframe <- used_addresses(prev_samples = usedaddresses,
                             paf = clean_paf)
nrow(scjs.sframe)

### 3 - Multiple occupancy  ---- 

# Add message to inform user about progress
message(normal("Create multiple occupancy indicator"))

scjs.sframe <- scjs.sframe %>% multiple_occupancy()
nrow(scjs.sframe)

### 4 - Sampling ---- 

# Add message to inform user about progress
message(normal("Draw sample"))

scjs.control <- c("dz11_urbrur2020",
                  "simd20rank",
                  "postcode",
                  "print_address")

# Draw stratified systematic sample
# As the selection probability of some addresses is zero,
# a warning message will be displayed when executing the code below.
# This is to be expected and nothing to be concerned about.
scjs.totalsample <- scjs.sframe %>%
  sampling(stratum = "la_code",
           sample_size = scjs.samplesize$total_n,
           prob = "totalsize",
           control = scjs.control)

# Merge sampling frame and drawn sample and sort data frame
scjs.frameandmatchedsample <- scjs.sframe %>% 
  merge_frame_sample(scjs.totalsample)

### 5 - Draw reserve sample ---- 

# Add message to inform user about progress
message(normal("Draw reserve sample"))

# Draw stratified systematic sample
scjs.reservesample <- scjs.totalsample %>% 
  sampling(stratum = "la_code",
           sample_size = scjs.samplesize$reserve_n,
           prob = rep(1/nrow(scjs.totalsample), 
                      times = nrow(scjs.totalsample)),
           control = scjs.control)

# Remove reserve sample from contractor sample
scjs.contractorsample <- anti_join(x = scjs.totalsample, 
                            y = scjs.reservesample,
                            by = "udprn")
nrow(scjs.contractorsample)

### 6 - Prepare for export ----

# Add message to inform user about progress
message(normal("Prepare for export"))

scjs.contractor.export <- scjs.contractorsample %>% prepare_for_export()

### 7 - Post-processing ---- 

# Add message to inform user about progress
message(normal("Post-process data"))

# generate streams 1:12
streams <- stream_allocation(1, 12)

# allocate generated streams
scjs.contractor.export$stream <- rep_len(streams$num, 
                                         length.out = nrow(scjs.contractor.export))

# Determine which households at multiple occupancy addresses gets interviewed
scjs.contractor.export <- scjs.contractor.export %>% selected_mo()

### 8 - Export sample  ----

# Code to export sampled addresses into output folder
# Current date is automatically added to file name to avoid 
# overwriting existing files

# Add message to inform user about progress
message(normal("Export sample"))

export_rds(scjs.totalsample)

export_rds(scjs.frameandmatchedsample)

export_rds(scjs.contractorsample)

export_rds(scjs.reservesample)

write.csv(scjs.contractor.export, 
          paste0(scjs.path,
                 "scjs.contractorsample.",
                 syear,
                 ".csv"),
          row.names = FALSE)

### END OF SCRIPT ####

# clear environment
rm(list=ls())
