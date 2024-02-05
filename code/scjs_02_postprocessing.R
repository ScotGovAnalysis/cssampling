#########################################################################
# Name of file - 2_scjs_postprocessing.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Draws the reserve sample of the SCJS, allocates a stream
# to each sampled address and determines which households at multiple 
# occupancy addresses get interviewed.

#########################################################################

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("code", "00_setup.R"))

### 1 - Import data ---- 

# Import drawn sample and select required columns
scjs.totalsample <- read_rds(paste0(scjs.path, 
                                    "scjs.initial.totalsample.",
                                    syear,
                                    ".rds")) %>%
  select(-c(id_unit, prob, stratum))

# Import reserve sample size and select required columns
scjs.reservesamplesize <- read_rds(paste0(scjs.path , 
                                          "scjs.initial.samplesize.",
                                          syear,
                                          ".rds")) %>%
  select(la_code, reserve_n)

### 2 - Draw reserve sample ---- 

# Sort sample frame prior to drawing the reserve sample
scjs.totalsample <- scjs.totalsample[order(scjs.totalsample$la_code,
                                           scjs.totalsample$dz11_urbrur2020,
                                           scjs.totalsample$simd20rank,
                                           scjs.totalsample$postcode,
                                           scjs.totalsample$print_address),]

# Draw reserve sample
scjs.initial.reservesample <- strata(data = scjs.totalsample, 
                               stratanames = c("la_code"), 
                               size = scjs.reservesamplesize$reserve_n, 
                               method = c("systematic"),
                               pik = rep(1/nrow(scjs.totalsample), 
                                         times = nrow(scjs.totalsample)))

scjs.initial.reservesample <- getdata(scjs.totalsample, 
                                      scjs.initial.reservesample) %>%
  clean_names_modified() %>% 
  select(-c(id_unit, prob, stratum))

# Remove reserve sample from contractor sample
scjs.initial.contractorsample <- anti_join(x = scjs.totalsample, 
                                           y = scjs.reservesample,
                                           by = "udprn")
nrow(scjs.initial.contractorsample)

# Select required columns and arrange data for contractor export
scjs.contractor.export <- scjs.initial.contractorsample %>%
  mutate(property = as.character(property)) %>%
  select(udprn, organisation, property, street, locality, town, 
         postcode, print_address, datazone, xcoord, ycoord, laa, 
         la_code, multisize, simd20rank, dz11_urbrur2020) %>%
  arrange(la_code, dz11_urbrur2020, simd20rank, postcode, print_address)


### 3 - Post-processing ---- 

# Randomly allocate 12 streams
num <- 1:12
rand <- runif(12)
streams <- as_tibble(cbind(num, rand))
streams <- streams[order(rand),]

scjs.contractor.export$stream <- rep_len(streams$num, 
                                         length.out = nrow(scjs.contractor.export))

# Determine which households at multiple occupancy addresses get interviewed
scjs.contractor.export <- scjs.contractor.export %>%
  mutate(rand = (runif(nrow(scjs.contractor.export), 0, 1)),
         selected_mo = round(rand*multisize + 0.5, 0)) %>%
  select(-rand)
  

### 4 - Export sample  ----

# Code to export contractor and reserve samples into output folder

write_rds(
  scjs.initial.contractorsample, 
  paste0(scjs.path,
         "scjs.initial.contractorsample.",
         syear,
         ".rds"),
  compress = "gz"
)

write_rds(
  scjs.initial.reservesample, 
  paste0(scjs.path, 
         "scjs.initial.reservesample.",
         syear,
         ".rds"),
  compress = "gz"
)

write.csv(scjs.contractor.export, 
          paste0(scjs.path,
                 "scjs.contractorsample.",
                 syear,
                 ".csv"),
          row.names = FALSE)

