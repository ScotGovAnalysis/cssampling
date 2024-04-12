#########################################################################
# Name of file - 02_used_addresses.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Imports all addresses sampled in the last four years 
# and exports them in a single file.

#########################################################################

# clear environment
rm(list=ls())

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("scripts", "00_setup.R"))

### 1 - Import files ----

# Import all previously sampled address files and 
# add column with file name
usedaddresses <-
  do.call(rbind, lapply(seq_along(previoussamples.path),
                        function(x)
                          transform(
                            haven::read_sas(previoussamples.path[x], 
                                            col_select = "UDPRN"),
                            survey = previoussamples.path[x])))

### 2 - Process data ----

# Mutate file name to name of survey and clean names
usedaddresses <- usedaddresses %>%
  mutate(survey = case_when(str_detect(survey, 
                                       regex("scjs",
                                             ignore_case = TRUE)) ~ "scjs",
                            str_detect(survey, 
                                       regex("shes", 
                                             ignore_case = TRUE)) ~ "shes",
                            str_detect(survey, 
                                       regex("shs", 
                                             ignore_case = TRUE)) ~ "shs",
                            str_detect(survey, 
                                       regex("ssas", 
                                             ignore_case = TRUE)) ~ "ssas")) %>%
  rename(udprn = UDPRN)

# Keep unique UDPRN and survey combination and select required variables
usedaddresses <- usedaddresses %>% 
  group_by(udprn, survey) %>% 
  count() %>% 
  select(udprn, survey)

### 3 - Export used addresses  ----

# Code to export used addresses into lookups folder
write_rds(
  usedaddresses,
  here("lookups", paste0(Sys.Date(), "_usedaddresses.rds")),
  compress = "gz"
)

### END OF SCRIPT ####

# clear environment
rm(list=ls())

