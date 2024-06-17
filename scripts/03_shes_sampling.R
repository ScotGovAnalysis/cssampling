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

# Add message to inform user about progress
message(title("Execute sampling script"))

### 1 - Import files ---- 

# Add message to inform user about progress
message(normal("Import data"))

# Identify most recent used addresses file
recent_usedaddresses <- cs_most_recent_file(path = here("lookups"), 
                                         pattern = "usedaddresses")

# Import used addresses
usedaddresses <- read_rds(paste0(here("lookups"), "/", recent_usedaddresses))

# Identify most recent cleaned PAF
recent_paf <- cs_most_recent_file(path = here("lookups"), pattern = "paf")

# Import cleaned PAF
clean_paf <- read_rds(paste0(here("lookups", "/", recent_paf)))

# Import SIMD ranks for datazones
dz11_simd20 <- haven::read_sas(dz_simd.path) %>%
  cs_clean_names_modified()

# Import sample size file
shes.samplesize <- read.csv(shes.samplesize.path, 
                            header = TRUE, na = "") %>%
  cs_clean_names_modified()

# Import SHeS cluster file
shes_clusters <- read.csv(shes.clusters.path, 
                          header = TRUE, na = "") %>%
  cs_clean_names_modified()

# import biomod file
biomodsframe <- read.csv(biomod.path, 
                         header = TRUE, na = "") %>%
  cs_clean_names_modified()

### 2 - Used addresses ---- 

# Add message to inform user about progress
message(normal("Add metrics for active addresses"))

shes.sframe <- cs_used_addresses(prev_samples = usedaddresses,
                              paf = clean_paf)
nrow(shes.sframe)

### 3 - Cluster size ---- 

# Add message to inform user about progress
message(normal("Add clusters"))

shes.sframe <- shes.sframe %>% 
  
  # remove all columns that contain "shes_y" 
  select(-contains("shes_y")) %>%
  
  # add SHeS cluster information
  left_join(shes_clusters,
            by = join_by(dz11, simd20rank)) %>%
  
  # create 'shes_clustersize' column based on shes.surveysweep
  mutate(across(
    .cols = contains(paste0("shes_y", 
                            shes.surveysweep), ignore.case = FALSE),
    .names = 'shes_clustersize'))
nrow(shes.sframe)

### 4 - Multiple occupancy  ---- 

# Add message to inform user about progress
message(normal("Create multiple occupancy indicator"))

shes.sframe <- shes.sframe %>% cs_multiple_occupancy()
nrow(shes.sframe)

### 5 - Sampling ---- 

# Add message to inform user about progress
message(normal("Draw sample"))

shes.control <- c("dz11_urbrur2020",
                  "average_simd20_rank",
                  "simd20rank",
                  "postcode",
                  "print_address")

# Draw stratified systematic sample
shes.totalsample <- shes.sframe %>%
  cs_sampling(stratum = "la_code",
           sample_size = shes.samplesize$total_n,
           prob = "totalsize",
           control = shes.control)

# Merge sampling frame and drawn sample and sort data frame
shes.frameandmatchedsample <- shes.sframe %>% 
  cs_merge_frame_sample(shes.totalsample)

### 6 - Draw reserve sample ---- 

# Add message to inform user about progress
message(normal("Draw reserve sample"))

# calculate total reserve sample size
shes.samplesize <- shes.samplesize %>% 
  mutate(total_reserve = select(., starts_with("reserve")) %>% rowSums())

shes.totalsample.modules <- shes.totalsample %>% 
  select(-c(prob, stratum, totalsize, activeflag, pactive))

# Draw reserve sample
shes.reservesample <- shes.totalsample.modules %>%
  cs_sampling(stratum = "la_code",
           sample_size = shes.samplesize$total_reserve,
           prob = rep(1/nrow(shes.totalsample.modules), 
                      times = nrow(shes.totalsample.modules)),
           control = shes.control)

# Remove reserve sample from contractor sample
shes.contractorsample <- anti_join(x = shes.totalsample, 
                                   y = shes.reservesample,
                                   by = "udprn")
nrow(shes.contractorsample)

### 7 - Draw biological module  ---- 

# Add message to inform user about progress
message(normal("Draw biological sample"))

# select which intermediate geographies (data zones in the islands) 
# will be subject to the biological module

biomod <- biomodsframe %>% filter(stream == shes.biomodstream)

# Draw stratified systematic sample

biomod.control <- c("health_board",
                    "local_authority",
                    "ur20",
                    "average_simd20_rank")

biomodsample <- biomod %>%
  cs_sampling(stratum = "stream",
              sample_size = biomodsamplesize,
              prob = rep(1/nrow(biomod), 
                         times = nrow(biomod)),
                         control = biomod.control)

shes.biomod.frameandmatchedsample <- suppressMessages(biomod %>% 
                                                        left_join(biomodsample)) %>%
  mutate(biomod = ifelse(is.na(prob) == TRUE, 0, 1)) %>%
  select(-c(prob, stratum)) %>%
  arrange(health_board, local_authority, ur20, average_simd20_rank)

### 8 - Draw child sample ---- 

# Add message to inform user about progress
message(normal("Draw child sample"))

# Split the sample into child sample frame and islands. 
# The islands do not have a child boost and therefore are not 
# included in the sample frame
islands <- shes.contractorsample %>% filter(la_code %in% c(235, 330, 360))
child.sframe <- shes.contractorsample %>% filter(!la_code %in% c(235, 330, 360))

# get sample size for child boost (exclude islands)
child.samplesize <- shes.samplesize %>% filter(!shes_strata %in% c(235, 330, 360))

# Select the child boost sample from the contractor sample
child.mainsample  <- child.sframe %>%
  cs_sampling(stratum = "la_code",
           sample_size = child.samplesize$child_n,
           prob = rep(1/nrow(child.sframe), 
                      times = nrow(child.sframe)),
           control = shes.control)

nrow(child.mainsample)

### 9 - Contractor sample ---- 

# Add message to inform user about progress
message(normal("Combine samples"))

# Recombine drawn sample with the islands sample and add the child_boost, 
# main and adult_boost flags

shes.full.contractorsample <- child.mainsample %>%
  
  # add child_boost flag
  mutate(child_boost = 1,
         core = 0) %>%
  
  # merge child sample with whole contractor sample
  right_join(shes.contractorsample,
             by = join_by(udprn),
             suffix = c('.x', '')) %>%
  select(-contains(".x")) %>%
  
  # replace NAs in child_boost flag with 0
  # (NAs were introduced for addresses who appeared in the whole contractor 
  # sample but not the child boost sample when the two data frames were merged)
  mutate(child_boost = ifelse(is.na(child_boost) == TRUE, 0, child_boost),
         core = ifelse(is.na(core) == TRUE, 1, core),
         hb_boost = 0) %>%
  
  # add biological module information
  left_join(shes.biomod.frameandmatchedsample, by = "cluster21")

nrow(shes.full.contractorsample)         

shes.full.contractorsample <- shes.full.contractorsample %>%
  
  # create sample_type column and assign values based on conditions
  mutate(sample_type = ifelse(hb_boost == 1, "HB Boost", NA),
         sample_type = ifelse(child_boost == 1, "Child Boost", sample_type),
         sample_type = ifelse(core == 1 & biomod != 1, "Core (NON) bio", sample_type),
         sample_type = ifelse(core == 1 & biomod == 1, "Core Bio", sample_type),
         
         # create core_bio column and assign values based on conditions
         core_bio = ifelse(1 %in% c(hb_boost, child_boost), 0, NA),
         core_bio = ifelse(core == 1 & biomod == 1, 1, core_bio),
         core_bio = ifelse(core == 1 & biomod != 1, 0, core_bio),
  )

# inspect contractor sample
shes.full.contractorsample %>% 
  group_by(la_code, sample_type) %>%
  count() %>% 
  pivot_wider(names_from = sample_type, 
              id_cols = la_code, 
              values_from = n,
              values_fill = 0) %>%
  adorn_totals("row") %>%
  adorn_totals("col")

### 10 - Prepare for export ----

# Add message to inform user about progress
message(normal("Prepare for export"))

shes.contractorsample.export <- cs_prepare_for_export(shes.full.contractorsample)

### 11 - Export sample  ----

# Add message to inform user about progress
message(normal("Export sample"))

# Code to export sampled addresses into output folder
# Current date is automatically added to file name to avoid 
# overwriting existing files

cs_export_rds(shes.totalsample)

cs_export_rds(shes.samplesize)

cs_export_rds(shes.frameandmatchedsample)

cs_export_rds(shes.biomod.frameandmatchedsample)

cs_export_rds(shes.full.contractorsample)

cs_export_rds(shes.reservesample)

write.csv(shes.contractorsample.export, 
          paste0(shes.path,
                 "shes.contractorsample.",
                 syear,
                 ".csv"),
          row.names = FALSE)

### END OF SCRIPT ####

# clear environment
rm(list=ls())
