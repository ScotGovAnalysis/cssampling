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

# check if sampling script has been run and print error if not
if(length(list.files(path = scjs.path,
                     pattern = "scjs.frameandmatchedsample")) == 0){
  stop(print("03_scjs_sampling.R needs to be run first."))
}

# Add message to inform user about progress
message(title("Execute checking script"))

### 1 - Import data ---- 

# Add message to inform user about progress
message(normal("Import data"))

# Identify most recent sampling frame matched with sample
recent_frameandmatchedsample <- css_most_recent_file(path = scjs.path, 
                                         pattern = "scjs.frameandmatchedsample")

# Import sampling frame matched with sample
scjs.frameandmatchedsample <- read_rds(paste0(scjs.path, "/", 
                                              recent_frameandmatchedsample))


# Identify most recent contractor sample
sample_fname <- paste0("_scjs.contractorsample.",
                       config$syear,
                       ".csv")
recent.contractor.sample <- css_most_recent_file(path = scjs.path, 
                                                 pattern = sample_fname)

# Import contractor sample
contractor.sample <- read.csv(paste0(scjs.path,
                                     recent.contractor.sample)) %>%
  css_clean_names_modified()

# Import sample size information
sample.size <- read.csv(config$scjs.samplesize.path, 
                        header = TRUE, na = "") %>%
  css_clean_names_modified()

# Import data zones and select required columns
dz_info <- haven::read_sas(config$dz.path) %>%
  css_clean_names_modified() %>% 
  select(la, dz11)

# Import previous year's contractor sample
contractor.sample.previous <- read.csv(config$scjs.contractor.sample.previous.path) %>%
  css_clean_names_modified()

# Import household estimates by datazone
last_sheet <- length(excel_sheets(config$hh_dz.path))
hh.est.dz <- suppressWarnings(read_excel(config$hh_dz.path, 
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
contractor.sample.size.check <- css_check_sample_size(
  df = contractor.sample,
  sample.size = sample.size
)

### 4 - Check for previously sampled addresses ----

# Add message to inform user about progress
message(normal("Check for previously sampled addresses"))

# Import previously sampled and delivered UDPRNs
udprn.qa <- css_delivered_udprn(sampling_year = config$syear,
                            filepath = config$datashare.path)

### 5 - Mean SIMD for sample & sampling frame by local authority ----

# Add message to inform user about progress
message(normal("Mean SIMD for sample & sampling frame by local authority"))

simd.qa <- css_check_mean_simd(total.sample, paf, grouping_variable = la)

### 6 - Urban/rural classification ----

# Add message to inform user about progress
message(normal("Urban/rural classification"))

urbrur.la.qa <- css_check_urbrur(scjs.frameandmatchedsample)

### 7 - Check postcodes ----

# Add message to inform user about progress
message(normal("Check postcodes"))

pcode <- css_check_postcodes(total.sample)

### 8 - Check business addresses ----

# Add message to inform user about progress
message(normal("Check business addresses"))

business.qa <- css_check_businesses(sample = total.sample)

### 9 - Check multisize distribution ----

# Add message to inform user about progress
message(normal("Check multisize distribution"))

multisize.qa <- css_check_multisize(sample = total.sample, paf = paf)

### *CONTRACTOR SAMPLE* ----

### 9 - Check SIMD in contractor sample ----

# Add message to inform user about progress
message(normal("Check SIMD"))

contractor.simd.qa <- css_check_contractor_simd(sample = contractor.sample, 
                                            paf.simd = simd.qa[[3]],
                                            grouping_variable = la)

### 10 - Check business addresses in contractor sample ----

# Add message to inform user about progress
message(normal("Check business addresses in contractor sample"))

css_check_contractor_businesses(contractor.sample)

### 11 - Check stream allocation in contractor sample ----

# Add message to inform user about progress
message(normal("Check stream allocation"))

contractor.stream.qa <- css_check_stream(sample = contractor.sample,
                                     grouping_variable = la)

### 12 - Check data zones in contractor sample ----

# Add message to inform user about progress
message(normal("Check data zones"))

contractor.datazone.qa <- css_check_contractor_datazones(sample = contractor.sample,
                                                     dz = dz_info,
                                                     hh.estimates = hh.est.dz)

### 13 - Check SIMDQ in contractor sample ----

contractor.simdq.qa <- css_check_contractor_simdq(sample = contractor.sample,
                                              previous.sample = contractor.sample.previous)
    
### 14 - Check urbrur in contractor sample ----

# Add message to inform user about progress
message(normal("Check urbrur"))

contractor.urbrur.qa <- css_check_contractor_urbrur(sample = contractor.sample,
                                                previous.sample = contractor.sample.previous)

### *EXPORT* ----

### 15 - Export checks to excel file for manual inspection  ----

# Add message to inform user about progress
message(normal("Export"))

# create cover data frame
cover <- tibble::tribble(~sheet, ~explanation,
                         "contractor.sample", "This sheet shows the drawn contractor sample.")
cover <- rbind(cover,
               contractor.sample.size.check[[1]],
               udprn.qa[[1]],
               simd.qa[[1]],
               urbrur.la.qa[[1]],
               pcode[[1]],
               business.qa[[1]],
               multisize.qa[[1]],
               contractor.simd.qa[[1]],
               contractor.stream.qa[[1]],
               contractor.datazone.qa[[1]],
               contractor.simdq.qa[[1]],
               contractor.urbrur.qa[[1]],
               contractor.urbrur.qa[[2]])

# Create list of all objects to be exported
qa <- list(cover = cover,
           contractor.sample = contractor.sample,
           contractor.sample.size = contractor.sample.size.check[[2]],
           previously.sampled.udprn = udprn.qa[[2]],
           simd.la = simd.qa[[4]],
           urbrur.la = urbrur.la.qa[[2]],
           sampled.postcodes = pcode[[2]],
           business.addresses = business.qa[[2]],
           multisize = multisize.qa[[2]],
           contractor.simd.la = contractor.simd.qa[[2]],
           contractor.stream.la = contractor.stream.qa[[2]],
           contractor.datazone = contractor.datazone.qa[[2]],
           contractor.simdq.la = contractor.simdq.qa[[2]],
           contractor.urbrur = contractor.urbrur.qa[[4]],
           contractor.urbrur.la = contractor.urbrur.qa[[3]])

# Export to Excel

css_qa_export(list_df = qa,
          survey = survey)

### END OF SCRIPT ####

# clear environment
rm(list=ls())
