#########################################################################
# Name of file - shes_02_biomod.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Selects the intermediate geographies eligible for the
# biological module, draws the reserve sample, draws the child boost sample,
# cleans the contractor sample and exports it.

#########################################################################

### 0 - Setup ----

# Run setup script which loads all required packages and 
# functions and executes the config.R script.

source(here::here("code", "00_setup.R"))

### 1 - Import files ---- 

biomodsframe <- read.csv(biomod.path, 
                         header = TRUE, na = "") %>%
  clean_names_modified()

### 2 - Biological module  ---- 

# select which intermediate geographies (data zones in the islands) 
# will be subject to the biological module

biomod <- biomodsframe %>% filter(stream == shes.biomodstream)

biomod  <- biomod[order(biomod$stream,
                        biomod$health_board,
                        biomod$local_authority,
                        biomod$ur20,
                        biomod$average_simd20_rank),]

# Draw stratified systematic sample
biomodsample <- strata(data = biomod,
                       stratanames = c("stream"),
                       size = biomodsamplesize, 
                       method = c("systematic"), 
                       pik = rep(1/nrow(biomod), 
                                 times = nrow(biomod)))

biomodsample <- getdata(biomod, biomodsample) %>% 
  clean_names_modified()


biomod.frameandmatchedsample <- biomod %>% 
  left_join(biomodsample) %>%
  mutate(biomod = ifelse(is.na(prob) == TRUE, 0, 1)) %>%
  select(-c(prob, stratum)) %>%
  arrange(health_board, local_authority, ur20, average_simd20_rank)

### 3 - Draw reserve sample ---- 

# calculate total reserve sample size
shes.samplesize <- shes.samplesize %>% 
  mutate(total_reserve = select(., starts_with("reserve")) %>% rowSums())

shes.totalsample.modules <- shes.totalsample %>% 
  select(-c(prob, stratum, totalsize, activeflag, pactive))


shes.totalsample.modules  <- shes.totalsample.modules[order(shes.totalsample.modules$la_code,
                                                            shes.totalsample.modules$dz11_urbrur2020,
                                                            shes.totalsample.modules$average_simd20_rank,
                                                            shes.totalsample.modules$simd20rank,
                                                            shes.totalsample.modules$postcode,
                                                            shes.totalsample.modules$print_address),]

# Draw stratified systematic sample
shes.reservesample <- strata(data = shes.totalsample.modules,
                             stratanames = c("la_code"),
                             size = shes.samplesize$total_reserve, 
                             method = c("systematic"), 
                             pik = rep(1/nrow(shes.totalsample.modules), 
                                       times = nrow(shes.totalsample.modules)))

shes.reservesample <- getdata(shes.totalsample.modules, shes.reservesample) %>% 
  clean_names_modified()

nrow(shes.reservesample)


# Remove reserve sample from contractor sample
shes.contractorsample <- anti_join(x = shes.totalsample, 
                                   y = shes.reservesample,
                                   by = "udprn")
nrow(shes.contractorsample)


### 4 - Draw child sample ---- 

# Split the sample into child sample frame and islands. 
# The islands do not have a child boost and therefore are not 
# included in the sample frame
islands <- shes.contractorsample %>% filter(la_code %in% c(235, 330, 360))
child.sframe <- shes.contractorsample %>% filter(!la_code %in% c(235, 330, 360))

# get sample size for child boost (exclude islands)
child.samplesize <- shes.samplesize %>% filter(!shes_strata %in% c(235, 330, 360))

# Select the child boost sample from the contractor sample
child.sframe  <- child.sframe[order(child.sframe$la_code,
                                    child.sframe$dz11_urbrur2020,
                                    child.sframe$average_simd20_rank,
                                    child.sframe$simd20rank,
                                    child.sframe$postcode,
                                    child.sframe$print_address),]

# Draw stratified systematic sample
child.mainsample <- strata(data = child.sframe,
                           stratanames = c("la_code"),
                           size = child.samplesize$child_n, 
                           method = c("systematic"), 
                           pik = rep(1/nrow(child.sframe), 
                                     times = nrow(child.sframe)))

child.mainsample <- getdata(child.sframe, child.mainsample) %>% 
  clean_names_modified()

nrow(child.mainsample)

### 6 - Contractor sample ---- 

# Recombine drawn sample with the islands sample and add the child_boost, 
# main and adult_boost flags

shes.full.contractorsample <- child.mainsample %>%
  
  # add child_boost flag
  mutate(child_boost = 1,
         core = 0) %>%
  
  # merge child sample with whole contractor sample
  right_join(shes.contractorsample) %>%
  
  # replace NAs in child_boost flag with 0
  # (NAs were introduced for addresses who appeared in the whole contractor 
  # sample but not the child boost sample when the two data frames were merged)
  mutate(child_boost = ifelse(is.na(child_boost) == TRUE, 0, child_boost),
         core = ifelse(is.na(core) == TRUE, 1, core),
         hb_boost = 0) %>%
  
  # add biological module information
  left_join(biomod.frameandmatchedsample, by = "cluster21")

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
              values_from = n) %>%
  adorn_totals("row") %>%
  adorn_totals("col")

### 7 - Prepare for export ----

shes.contractorsample.export <- prepare_for_export(shes.full.contractorsample)

### 8 - Export sample  ----

export_rds(shes.full.contractorsample)

export_rds(shes.reservesample)

write.csv(shes.contractorsample.export, 
          paste0(shes.path,
                 "shes.contractorsample.",
                 syear,
                 ".csv"),
          row.names = FALSE)

