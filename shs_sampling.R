#########################################################################
# Name of file - shs_sampling.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Calls all scripts needed to draw a Scottish Household
# Survey sample. Cleans the postcode address file (PAF) if this 
# hasn't been done already, identifies all previously sampled addresses, 
# draws the sample, post-processes the sample and checks the sample.

#########################################################################

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("scripts", "00_setup.R"))

### 1 - PAF ----

# Check if PAF script has been run with most recent PAF file
# If it hasn't been run, run PAF script
if(!any(grepl(paf_v, paf_list))){
  source(here::here("scripts", "01_paf.R"))
}

### 2 - Previously sampled addresses ----

# Run the used addresses script to identify all previously sampled addresses
source(here::here("scripts", "02_used_addresses.R"))

### 3 - Sampling ----

# Run the sampling script to draw the sample and post-process it
source(here::here("scripts", "03_shs_sampling.R"))

### 4 - Sample checking ----

# Run the checking script to confirm the drawn sample meets requirements
source(here::here("scripts", "04_shs_checking.R"))

