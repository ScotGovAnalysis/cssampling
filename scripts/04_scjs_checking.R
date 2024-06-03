#########################################################################
# Name of file - 04_scjs_checking.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Performs checks on the contractor sample as well as
# the total sample. Results are exported to an excel file. Relevant
# columns are conditionally formatted to facilitate manual inspection.

#########################################################################

# clear environment
rm(list=ls())

# indicate what survey is being checked
survey <- "scjs"

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message(title("Execute checking script"))

### 1 - Import data ---- 

# Add message to inform user about progress
message(normal("Import data"))

# Identify most recent sampling frame matched with sample
recent_frameandmatchedsample <- most_recent_file(path = scjs.path, 
                                         pattern = "scjs.frameandmatchedsample")

# Import sampling frame matched with sample
scjs.frameandmatchedsample <- read_rds(paste0(scjs.path, "/", 
                                              recent_frameandmatchedsample))

# Import contractor sample
contractor.sample <- read.csv(paste0(scjs.path,
       "scjs.contractorsample.",
       syear,
       ".csv")) %>%
  clean_names_modified()

# Import sample size information
sample.size <- read.csv(scjs.samplesize.path, 
                        header = TRUE, na = "") %>%
  clean_names_modified()

# Import data zones and select required columns
dz_info <- haven::read_sas(dz.path) %>%
  clean_names_modified() %>% 
  select(la, dz11)

# Import previous year's contractor sample
contractor.sample.previous <- read.csv(scjs.contractor.sample.previous.path) %>%
  clean_names_modified()

# Import household estimates by datazone
last_sheet <- length(excel_sheets(hh_dz.path))
hh.est.dz <- suppressWarnings(read_excel(hh_dz.path, 
                        sheet = last_sheet,
                        skip = 3) %>%
  clean_names(replace = c("Data Zone code" = "datazone")) %>%
  select(c("datazone", "occupied_dwellings")))

### 2 - Add indicator for sampled addresses ---- 

# Add message to inform user about progress
message(normal("Add indicator for sampled addresses"))

# Add indicator for sampled addressed 
# ('Yes' = sampled, 'No' = not sampled)
scjs.frameandmatchedsample <- scjs.frameandmatchedsample %>%
  mutate(selected = ifelse(is.na(stratum) != TRUE, "Yes", "No"))

# Subset sample
total.sample <- scjs.frameandmatchedsample %>% filter(selected == "Yes")
nrow(total.sample)

# Subset addresses whose selection probability is not 0
paf <- scjs.frameandmatchedsample %>% filter(totalsize != 0)
nrow(paf)

### *TOTAL SAMPLE* ----

### 3 - Check sample size requirements ----

# Add message to inform user about progress
message(normal("Check sample size requirements"))

# Compare sample size requirements with drawn sample
contractor.sample.size.check <- check_sample_size(
  df = contractor.sample,
  sample.size = sample.size
)

### 4 - Check for previously sampled addresses ----

# Add message to inform user about progress
message(normal("Check for previously sampled addresses"))

# Import previously sampled and delivered UDPRNs
udprn.qa <- delivered_udprn(sampling_year = syear,
                            filepath = datashare.path)

### 5 - Mean SIMD for sample & sampling frame by local authority ----

# Add message to inform user about progress
message(normal("Mean SIMD for sample & sampling frame by local authority"))

simd.qa <- check_mean_simd(total.sample, paf, grouping_variable = la)

### 6 - Urban/rural classification ----

# Add message to inform user about progress
message(normal("Urban/rural classification"))

urbrur.la.qa <- check_urbrur(scjs.frameandmatchedsample)

### 7 - Check postcodes ----

# Add message to inform user about progress
message(normal("Check postcodes"))

pcode <- check_postcodes(total.sample)

### 8 - Check business addresses ----

# Add message to inform user about progress
message(normal("Check business addresses"))

business.qa <- check_businesses(sample = total.sample)

### 9 - Check multisize distribution ----

# Add message to inform user about progress
message(normal("Check multisize distribution"))

multisize.qa <- check_multisize(sample = total.sample, paf = paf)

### *CONTRACTOR SAMPLE* ----

### 9 - Check SIMD in contractor sample ----

# Add message to inform user about progress
message(normal("Check SIMD"))

contractor.simd.qa <- check_contractor_simd(sample = contractor.sample, 
                                            paf.simd = simd.qa[[2]],
                                            grouping_variable = la)

### 10 - Check business addresses in contractor sample ----

# Add message to inform user about progress
message(normal("Check business addresses in contractor sample"))

check_contractor_businesses(contractor.sample)

### 11 - Check stream allocation in contractor sample ----

# Add message to inform user about progress
message(normal("Check stream allocation"))

contractor.stream.qa <- check_stream(sample = contractor.sample,
                                     grouping_variable = la)

### 12 - Check data zones in contractor sample ----

# Add message to inform user about progress
message(normal("Check data zones"))

contractor.datazone.qa <- check_contractor_datazones(sample = contractor.sample,
                                                     dz = dz_info,
                                                     hh.estimates = hh.est.dz)

### 13 - Check SIMDQ in contractor sample ----

contractor.simdq.qa <- check_contractor_simdq(sample = contractor.sample,
                                              previous.sample = contractor.sample.previous)
    
### 14 - Check urbrur in contractor sample ----

# Add message to inform user about progress
message(normal("Check urbrur"))

contractor.urbrur.qa <- check_contractor_urbrur(sample = contractor.sample,
                                                previous.sample = contractor.sample.previous)

### *EXPORT* ----

### 15 - Export checks to excel file for manual inspection  ----

# Add message to inform user about progress
message(normal("Export"))

# Create list of all objects to be exported
qa <- list(contractor.sample = contractor.sample,
        contractor.sample.size = contractor.sample.size.check,
        previously.sampled.udprn = udprn.qa,
        simd.la = simd.qa[[3]],
        urbrur.la = urbrur.la.qa,
        sampled.postcodes = pcode,
        business.addresses = business.qa,
        multisize = multisize.qa,
        contractor.simd.la = contractor.simd.qa,
        contractor.stream.la = contractor.stream.qa,
        contractor.datazone = contractor.datazone.qa,
        contractor.simdq.la = contractor.simdq.qa,
        contractor.urbrur = contractor.urbrur.qa[[2]],
        contractor.urbrur.la = contractor.urbrur.qa[[1]])

# Export to Excel

qa_export(list_df = qa,
          survey = survey)

### END OF SCRIPT ####

# clear environment
rm(list=ls())
