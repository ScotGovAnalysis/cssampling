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

# Add message to inform user about progress
message("Execute checking script")

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("scripts", "00_setup.R"))

### 1 - Import data ---- 

# Add message to inform user about progress
message("   Import data")

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
  select(laa, dz11)

# Import previous year's contractor sample
contractor.sample.previous <- read.csv(scjs.contractor.sample.previous.path) %>%
  clean_names_modified()

# Import SIMD ranks for datazones
dz11_simd20 <- haven::read_sas(dz_simd.path) %>%
  clean_names_modified()

# Import household estimates by datazone
last_sheet <- length(excel_sheets(hh_dz.path))
hh.est.dz <- read_excel(hh_dz.path, 
                        sheet = last_sheet,
                         skip = 3) %>%
  clean_names(replace = c("Data Zone code" = "datazone")) %>%
  select(c("datazone", "occupied_dwellings"))
  

### 2 - Add indicator for sampled addresses ---- 

# Add message to inform user about progress
message("   Add indicator for sampled addresses")

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

# Add message to inform user about progress
message("   Total sample")

### 3 - Check sample size requirements ----

# Add message to inform user about progress
message("      Check sample size requirements")

# Compare sample size requirements with drawn sample
contractor.sample.size.check <- contractor.sample %>% 
  group_by(la_code) %>% 
  summarise(drawn_n = n()) %>%
  merge(sample.size, by = "la_code") %>%
  select(la_code, drawn_n, contractor_n) %>%
  mutate(diff = contractor_n - drawn_n)

### 4 - Check for previously sampled addresses ----

# Add message to inform user about progress
message("      Check for previously sampled addresses")

# Import previously sampled and delivered UDPRNs
prev.samples <- delivered_udprn(sampling_year = syear,
                                filepath = datashare.path)

# Check if any of the recently sampled UDPRNs has been previously delivered
udprn.qa <- prev.samples[prev.samples$udprn %in% contractor.sample$udprn]
udprn.qa

### 5 - Mean SIMD for sample & sampling frame by local authority ----

# Add message to inform user about progress
message("      Mean SIMD for sample & sampling frame by local authority")

# Calculate SIMD statistics for sample
sample.simd <- total.sample %>% 
  group_by(laa) %>% 
  summarise(mean_sample = mean(simd20rank),
            median_sample = median(simd20rank),
            n_sample = n(),
            sd = sd(simd20rank)) %>%
  mutate(se = sd / sqrt(n_sample),
         lower.ci_sample = mean_sample - qt(1 - (0.05 / 2), n_sample - 1) * se,
         upper.ci_sample = mean_sample + qt(1 - (0.05 / 2), n_sample - 1) * se) %>%
  select(-c(sd, se))

# calculate SIMD statistics for PAF
paf.simd <- paf %>% 
  group_by(laa) %>%
  summarise(mean_paf = weighted.mean(simd20rank, totalsize),
            median_paf = median(simd20rank),
            n_paf = n(),
            .groups = 'drop')

# Merge SIMD statistics of PAF and sample
simd.qa <- merge(sample.simd, paf.simd, 
              by = "laa") %>%
  mutate(nsampnpaf = n_sample / n_paf,
         overlap = ifelse(lower.ci_sample < mean_paf & upper.ci_sample > mean_paf,
                          "no", "yes"))

### 6 - Urban/rural classification ----

# Add message to inform user about progress
message("      Urban/rural classification")

# Calculate urbrur percentage of sampled and non-sampled addresses
# in each local authority
urbrur.la.qa <- scjs.frameandmatchedsample %>%
  group_by(laa, dz11_urbrur2020, selected) %>%
  summarise(n = n(),
            .groups = 'drop') %>%
  group_by(laa, selected) %>%
  mutate(freq = n / sum(n) * 100) %>% 
  select(-n) %>%
  pivot_wider(names_from = selected, 
              names_prefix = "selected_",
              values_from = freq) %>%
  mutate(diff = selected_No - selected_Yes)

### 7 - Check postcodes ----

# Add message to inform user about progress
message("      Check postcodes")

# Add column indicating whether an address is part of the contractor sample 
# or reserve sample
pcode <- total.sample %>% 
  count(postcode) %>% 
  arrange(n) %>%
  mutate(allocation = ifelse(postcode %in% contractor.sample$postcode, 
                             "contractor", "reserve"))

# Check if any postcode has been sampled more than once and if so
# print warning
{
  if (head(pcode$n, 1) != 1)
  {stop("At least one postcode has been sampled more than once.")}
}
{
  if (tail(pcode$n, 1) != 1)
  {stop("At least one postcode has been sampled more than once.")}
}

### 8 - Check business addresses ----

# Add message to inform user about progress
message("      Check business addresses")

# Confirm that the number of businesses is low
# print warning if this isn't the case
{
  if(nrow(total.sample %>% filter(organisation != "")) >= 10)
  {stop("10 or more business addresses have been sampled (total sample).")}
}

# Inspect business addresses
business.qa <- total.sample %>% 
  filter(organisation != "") %>% 
  count(organisation)
business.qa

### 9 - Check multisize distribution ----

# Add message to inform user about progress
message("      Check multisize distribution")

# Check multi occupancy size for sample and PAF 
# Sample %s should be similar to PAF

contractor.multisize <- total.sample %>% 
  group_by(multisize) %>%
  summarise(contractor_n = n()) %>%
  mutate(contractor_perc = contractor_n/sum(contractor_n)*100)

total.multisize <- paf %>% 
  calc_perc(grouping_variable = multisize) %>%
  group_by(multisize) %>%
  count() %>%
  summarise(paf_n = n * multisize,
            paf_perc =  100 * paf_n/nrow(paf)) %>%
  mutate(cum_sum = cumsum(paf_n),
         cum_perc = cumsum(paf_perc))

multisize.qa <- merge(contractor.multisize[,1:3], 
                      total.multisize[,1:3],
                      by = "multisize", all = TRUE) %>%
  replace(is.na(.), 0) %>%
  mutate(diff = contractor_perc - paf_perc)

### *CONTRACTOR SAMPLE* ----

# Add message to inform user about progress
message("   Contractor sample")

### 9 - Check SIMD in contractor sample ----

# Add message to inform user about progress
message("      Check SIMD")

contractor.simd.qa <- contractor.sample %>% 
  group_by(laa) %>% 
  summarise(n = n(),
            mean_contractor = mean(simd20rank)) %>%
  merge(paf.simd, by = "laa") %>%
  select(-c(median_paf, n_paf)) %>%
  mutate(diff = mean_contractor/mean_paf-1)

### 10 - Check business addresses in contractor sample ----

# Add message to inform user about progress
message("      Check business addresses in contractor sample")

contractor.business <- contractor.sample %>% 
  filter(grepl('Business', print_address))
{
  if(nrow(contractor.business) >= 10)
  {stop("10 or more business addresses are in the contractor sample.")}
}

contractor.indust <- contractor.sample %>% 
  filter(grepl('Industrial', print_address))
{
  if(nrow(contractor.indust) >= 10)
  {stop("10 or more industrial addresses are in the contractor sample.")}
}

### 11 - Check stream allocation in contractor sample ----

# Add message to inform user about progress
message("      Check stream allocation")

# Checks allocation of sample across individual streams by LA
# For each local authority, calculate the total and mean number of 
# households per stream.
# The check column indicates the difference between the minimum and 
# the maximum values.
contractor.stream.qa <- contractor.sample %>%
  group_by(laa) %>%
  count(stream) %>%
  mutate(max = max(n),
         min = min(n)) %>%
  ungroup %>%
  pivot_wider(id_cols = c(laa, min, max), 
              names_from = stream, 
              values_from = n,
              values_fill = list(perc = 0)) %>%
  adorn_totals(c("col")) %>%
  mutate(mean = (Total-max-min)/12,
         check = max-min) %>%
  select(-c(max, min))

# check streams are equally distributed per local authority
{
  if(!all(contractor.stream.qa$check %in% c(0, 1)))
  {stop("Streams are not equally distributed across local authorities.")}
  }

### 12 - Check data zones in contractor sample ----

# Add message to inform user about progress
message("      Check data zones")

contractor.sample.dz <- contractor.sample %>% 
  group_by(datazone) %>%
  summarise(n_sample = n())

contractor.datazone.qa <- dz_info %>% 
  merge(contractor.sample.dz,
        by.x = "dz11",
        by.y = "datazone") %>%
  merge(hh.est.dz, 
        by.x = "dz11",
        by.y = "datazone")

contractor.datazone.qa <- contractor.datazone.qa %>% 
  group_by(laa) %>%
  mutate(perc_hh = occupied_dwellings / sum(occupied_dwellings),
         perc_sample = n_sample / sum(n_sample),
         diff = perc_hh - perc_sample)
contractor.datazone.qa

### 13 - Check SIMDQ in contractor sample ----

contractor.la.simdq <- contractor.sample %>%
  add_simdq() %>%
  la_grouping(grouping_variable = simdq)

contractor.previous.la.simdq <- contractor.sample.previous %>%
  add_simdq() %>%
  la_grouping(grouping_variable = simdq)

contractor.simdq.qa <- prev_cur_comp(current_df = contractor.la.simdq,
                            previous_df = contractor.previous.la.simdq)
    
### 14 - Check urbrur in contractor sample ----

# Add message to inform user about progress
message("      Check urbrur")

contractor.la.urbrur <- la_grouping(df = contractor.sample,
                                    grouping_variable = dz11_urbrur2020)

contractor.previous.la.urbrur <- la_grouping(df = contractor.sample.previous,
                                             grouping_variable = dz11_urbrur2020)
  
contractor.urbrur.la.qa <- prev_cur_comp(current_df = contractor.la.urbrur,
                                     previous_df = contractor.previous.la.urbrur)


# Compare number and percentages of sampled addresses per urbrur 
# classification with previous year

contractor.urbrur <- calc_perc(contractor.sample, 
                               dz11_urbrur2020)

contractor.previous.urbrur <- calc_perc(contractor.sample.previous, 
                                        dz11_urbrur2020)

contractor.urbrur.qa <- prev_cur_comp(contractor.urbrur, 
              contractor.previous.urbrur)

### *EXPORT* ----

### 15 - Export checks to excel file for manual inspection  ----

# Add message to inform user about progress
message("   Export")

# Create list of all objects to be exported
qa <- list(contractor.sample = contractor.sample,
        contractor.sample.size = contractor.sample.size.check,
        #contractor.simd.la = contractor.simd.distribution.qa,
        previously.sampled.udprn = udprn.qa,
        simd.la = simd.qa,
        urbrur.la = urbrur.la.qa,
        #pcode = pcode,
        business.addresses = business.qa,
        multisize = multisize.qa,
        contractor.simd.la = contractor.simd.qa,
        contractor.stream.la = contractor.stream.qa,
        contractor.datazone = contractor.datazone.qa,
        contractor.simdq.la = contractor.simdq.qa,
        contractor.urbrur = contractor.urbrur.qa,
        contractor.urbrur.la = contractor.urbrur.la.qa)

# Export to Excel

qa_export(list_df = qa,
          survey = "scjs")

### END OF SCRIPT ####

# clear environment
rm(list=ls())
