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

# Add message to inform user about progress
message(title("Execute used addresses script"))

### 1 - Import files ----

# Add message to inform user about progress
message(normal("Import data"))

# initiate list for previous samples
list_used_addresses <- list()

# Import all previously sampled address files and 
# add column with file name
if("previous.sas.samples.path" %in% names(config) == TRUE){
  prev.sas.samples <- config$previous.sas.samples.path %>%
    purrr::map(\(x) haven::read_sas(x, col_select = "UDPRN") %>%
                 mutate(filename = x), 
               .progress = TRUE) %>%
    purrr::list_rbind() %>%
    css_clean_names_modified()
  list_used_addresses <- c(list_used_addresses, list(prev.sas.samples))
}

# import overwritten SAS files
if("prev.csv" %in% names(config) == TRUE){
  prev.csv.samples <- config$prev.csv %>%
    purrr::map(\(x) css_import_multiple_files_csv(x), 
               .progress = TRUE) %>%
    purrr::list_rbind()
  list_used_addresses <- c(list_used_addresses, list(prev.csv.samples))
}

# import rds files (i.e., samples drawn with RAP)
if("previous.rap.samples.path" %in% names(config) == TRUE){
  files_prev_rap <- list.files(path = config$datashare.path,
                               pattern = capture.output(cat(config$previous.rap.samples.path, sep = "|")),
                               full.names = TRUE,
                               recursive = TRUE,
                               ignore.case = TRUE)
  
  prev.rap.samples.data <- seq_along(files_prev_rap) %>%
    purrr::map(\(x) transform(readRDS(files_prev_rap[x])) %>%
                 mutate(filename = files_prev_rap[x]), 
               .progress = TRUE) %>%
    purrr::list_rbind() %>%
    select(udprn, filename)
  list_used_addresses <- c(list_used_addresses, list(prev.rap.samples.data))
}

# combine udprns from all sources
usedaddresses <- do.call('rbind', list_used_addresses)

### 2 - Process data ----

# Add message to inform user about progress
message(normal("Process data"))

# Mutate file name to name of survey and clean names
usedaddresses <- usedaddresses %>%
  mutate(survey = case_when(str_detect(filename, 
                                       regex("scjs",
                                             ignore_case = TRUE)) ~ "scjs",
                            str_detect(filename, 
                                       regex("shes", 
                                             ignore_case = TRUE)) ~ "shes",
                            str_detect(filename, 
                                       regex("shs", 
                                             ignore_case = TRUE)) ~ "shs",
                            str_detect(filename, 
                                       regex("ssas", 
                                             ignore_case = TRUE)) ~ "ssas"))

# Keep unique UDPRN and survey combination and select required variables
usedaddresses <- usedaddresses %>% 
  group_by(udprn, survey) %>% 
  count() %>% 
  select(udprn, survey)

### 3 - Export used addresses  ----

# Add message to inform user about progress
message(normal("Export used addresses"))

# Code to export used addresses into lookups folder
write_rds(
  usedaddresses,
  here("lookups", paste0(Sys.Date(), "_usedaddresses.rds")),
  compress = "gz"
)

### END OF SCRIPT ####

# clear environment
rm(list=ls())

