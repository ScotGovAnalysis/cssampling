#########################################################################
# Name of file - config-example.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Specifies file paths, file names and 
# previously sampled addresses required for running the cssampling RAP.
# This is the only file which requires manual changes before the 
# RAP process is run. It is not pushed to git as it contains 
# sensitive information.
#########################################################################

### 0 - Seed - TO UPDATE ----
# The seed needs to be updated once a year.
# A random number should be generated using the following code:
# sample(1:1000000, 1)
set.seed(SEED_NUMBER)

### 1 - Sample year - TO UPDATE ----

# Year sample is being taken for (usually the coming year)
# (i.e., 20XX)
syear <- 20XX

### 2 - File paths - TO UPDATE ----

# This section only needs to be updated if the data storage location has changed
# (e.g., once the SAS server has been shut down)

# Path to data share
datashare.path <- "DATA_PATH"

# Path to SAS data
sasdata.path <- "SAS_PATH"

### 3 - File names - TO UPDATE ----

# File path of most recent postcode address file (PAF) supplied by NRS
infilenm.path <- "PAF_PATH"

# File path of postcode file with old addresses.
# This file is updated twice a year by NRS.
# The most recent version should be used.
# Data can be accessed via the following link:
# https://www.nrscotland.gov.uk/statistics-and-data/geography/nrs-postcode-extract
pcd.path <- "OLD_PAF_PATH"

# File path of SIMD rank information for each datazone
dz_simd.path <- "SIMD_PATH"

# File path of datazone information.
# DZ_info is a dataset created by Alex Stannard that has all datazones, 
# their LA, urb/rur status, deprivation, cluster for health survey.
# The file is should be updated regularly.
# The most recent version should be used. 
dz.path <- "DATAZONE_PATH"

# File path of most recent household estimates by datazone.
# This file is updated by NRS once a year. 
# The most recent version should be used.
# Data can be accessed here:
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings
hh_dz.path <- "HH_ESTIMATES_PATH"

### 3a - File names SCJS - TO UPDATE ----

# File path of SCJS sample size file
scjs.samplesize.path <- "SCJS_SAMPLE_SIZE_PATH"

# File path of previous year's SCJS contractor sample
scjs.contractor.sample.previous.path <- "SCJS_PREV_CONTRACTOR_SAMPLE_PATH"

### 3b - File names SHS - TO UPDATE ----

# File path of SHS sample size file
shs.samplesize.path <- "SHS_SAMPLE_SIZE_PATH"

# File path of previous year's SHS contractor sample
shs.contractor.sample.previous.path <- "SHS_PREV_CONTRACTOR_SAMPLE_PATH"

### 3c - File names SHeS - TO UPDATE ----

# File path of SHeS sample size file
shes.samplesize.path <- "SHES_SAMPLE_SIZE_PATH"

# File path of previous year's SHeS contractor sample
shes.contractor.sample.previous.path <- "SHES_PREV_CONTRACTOR_SAMPLE_PATH"

# File path of SHeS strata file
# Clusters need changing every 4 years. 
# This batch relates to 2020-2023.
shes.strata.path <- "SHES_STRATA_PATH"

# File path of SHeS clusters
shes.clusters.path <- "SHES_CLUSTERS_PATH"

# File path for SHeS biomod
biomod.path <- "SHES_BIOMOD_PATH"

# SHeS Survey Sweep
# 1 = 2021, 2 = 2022, 3 = 2023, 4 = 2024
# 1 = 2025, 2 = 2026, etc
shes.surveysweep <- rep(1:4, 100)[syear-2021+1]

# SHeS Bio mod stream
# A = 2021, B = 2022, C = 2023, D = 2024
# A = 2025, B = 2026, etc
shes.biomodstream <- rep(c("A", "B", "C", "D"), 100)[syear-2021+1]

# SHeS biomod sample size
biomodsamplesize <- VALUE

# SHeS Fife child boost
fife.childboost <- VALUE

### 4 - Used addresses - TO UPDATE ----

# Names of files with drawn samples of previous four years.

# At the beginning of a new year, samples that were drawn over four years ago 
# should be removed form the list below.
# For example, at the beginning of 2024, all samples drawn in 2020 
# should be removed.

# Initially, the total sample should be added below.
# Once each sweep of fieldwork is completed, the address files should
# be changed from TOTAL to CONTRACTOR, unless the reserve sample is
# actually used.

# previous samples drawn in SAS
previous.sas.samples <- c(
  # SYEAR-4 SAMPLES
  "NAME_OF_SHES_SAMPLE_SYEAR-4",
  "NAME_OF_SCJS_SAMPLE_SYEAR-4",
  "NAME_OF_SHS_SAMPLE_SYEAR-4",
  # SYEAR-3 SAMPLES
  "NAME_OF_SHES_SAMPLE_SYEAR-3",
  "NAME_OF_SCJS_SAMPLE_SYEAR-3",
  "NAME_OF_SHS_SAMPLE_SYEAR-3",
  # SYEAR-2 SAMPLES
  "NAME_OF_SHES_SAMPLE_SYEAR-2",
  "NAME_OF_SCJS_SAMPLE_SYEAR-2",
  "NAME_OF_SHS_SAMPLE_SYEAR-2",
  # SYEAR-1 SAMPLES
  "NAME_OF_SHES_SAMPLE_SYEAR-1",
  "NAME_OF_SCJS_SAMPLE_SYEAR-1",
  "NAME_OF_SHS_SAMPLE_SYEAR-1"
)

# get paths of all previously sampled address files
previous.sas.samples.path <- paste0(sasdata.path, previous.sas.samples, ".sas7bdat")

# add path to samples that were accidentally overwritten in SAS
prev.csv <- "OVERWRITTEN_SAMPLE_PATH"

# previous samples drawn in R
previous.rap.samples.path <- c(
  # SYEAR SAMPLES
  "NAME_OF_SHES_SAMPLE_SYEAR",
  ...
)

previous.rap.samples.path <- paste0(previous.rap.samples.path, ".rds")

### 5 - Thresholds for checking ----

# The following thresholds are used in the checking process to confirm that
# the the summary statistics of the drawn sample are within a certain range.
# When the sample values are not within the provided threshold, a warning
# will be displayed.
# Due to the different sample design, thresholds for the SHeS may differ
# from the other two surveys.
# The values usually don't need to be updated every year.

# Acceptable number of business addresses in the sample
business.threshold <- VALUE

# % difference between PAF and sample
paf_sample.threshold <- VALUE

# % difference of n across streams in SHS
shs.stream.threshold <- VALUE

# Difference of n across streams in SCJS
scjs.stream.threshold <- VALUE

# Difference between current and previous samples for SIMDQ variable
simdq.threshold <- VALUE

# % difference between current and previous samples for SIMDQ variable in SHeS
shes.simdq.threshold <- VALUE

# Acceptable number of samples per postcode
postcode.threshold <- VALUE

# Acceptable number of SHeS samples per postcode
shes.postcode.threshold <- VALUE

# % difference between PAF and sample AND previous and current sample
# for urban/rural in SHeS
shes.urbrur.threshold <- VALUE

