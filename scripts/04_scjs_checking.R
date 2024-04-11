#########################################################################
# Name of file - scjs_03_checking.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Performs checks on the contractor sample as well as
# the total sample. Results are exported to an excel file. Relevant
# columns are conditionally formatted to facilitate manual inspection.

#########################################################################

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("code", "00_setup.R"))

### 1 - Import data ---- 

# Import sampling frame matched with sample
scjs.frameandmatchedsample <- read_rds(paste0(scjs.path,
                        "scjs.frameandmatchedsample.",
                        syear,
                        ".rds")) %>%
  clean_names_modified()

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

# Compare sample size requirements with drawn sample
contractor.sample.size.check <- contractor.sample %>% 
  group_by(la_code) %>% 
  summarise(drawn_n = n()) %>%
  merge(sample.size, by = "la_code") %>%
  select(la_code, drawn_n, contractor_n) %>%
  mutate(diff = contractor_n - drawn_n)

### 4 - Check for previously sampled addresses ----

# Create variables with all survey names and four years prior to sampling year
usedaddressyears <- paste0(c(syear-4, syear-3, syear-2, syear-1, syear))
usedaddressyears <- capture.output(cat(usedaddressyears, sep = "|"))
surveys <- "(shs)|(shes)|(scjs)|(ssas)"

# Identify file names which contain the term 'delivered'
files_del <- list.files(path = datashare.path,
                        pattern = paste0(".*(", usedaddressyears,").*delivered.*\\.csv$"),
                        full.names = TRUE,
                        recursive = TRUE,
                        ignore.case = TRUE)
files_del

# Remove delivered but never used UDPRNs
never_used <- files_del[grepl("never used", files_del, 
                              ignore.case = TRUE)]
files_del <- files_del[!(files_del %in% never_used)]
files_del

# Import all delivered UDPRNs
prev.samples <- lapply(files_del, import_previous_samples)
prev.samples <- rbindlist(prev.samples)

# Check if any of the sampled UDPRNs has been previously delivered
udprn.qa <- prev.samples[prev.samples$udprn %in% contractor.sample$udprn]
udprn.qa


### 5 - Mean SIMD for sample & sampling frame by local authority ----

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

# Add column indicating whether an address is part of the contractor sample 
# or reserve sample
pcode <- total.sample %>% 
  count(postcode) %>% 
  arrange(n) %>%
  mutate(allocation = ifelse(postcode %in% contractor.sample$postcode, 
                             "contractor", "reserve"))

# Check if any postcode has been sampled more than once
head(pcode$n, 1) == 1
tail(pcode$n, 1) == 1

### 8 - Check business addresses ----

# Confirm that the number of businesses is low
nrow(total.sample %>% filter(organisation != "")) < 10

# Inspect business addresses
business.qa <- total.sample %>% 
  filter(organisation != "") %>% 
  count(organisation)
business.qa

### 9 - Check multisize distribution ----

# Check multi occupancy size for sample and PAF 
# Sample %s should be similar to PAF
contractor.multisize <- total.sample %>% 
  group_by(multisize) %>%
  summarise(contractor_n = n()) %>%
  mutate(contractor_perc = contractor_n/sum(contractor_n)*100)

total.multisize <- paf %>% 
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

### 9 - Check contractor sample for SIMD ----

contractor.simd.qa <- contractor.sample %>% 
  group_by(laa) %>% 
  summarise(n = n(),
            mean_contractor = mean(simd20rank)) %>%
  merge(paf.simd, by = "laa") %>%
  select(-c(median_paf, n_paf)) %>%
  mutate(diff = mean_contractor/mean_paf-1)

### 10 - Check contractor sample for business addresses ----

contractor.business <- contractor.sample %>% 
  filter(grepl('Business', print_address))
nrow(contractor.business) < 10

contractor.indust <- contractor.sample %>% 
  filter(grepl('Industrial', print_address))
nrow(contractor.indust) < 10

### 11 - Check contractor sample for stream allocation ----

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

# check streams are equally distributed per per local authority
all(contractor.stream.qa$check %in% c(0, 1))

### 12 - Check contractor sample for datazones ----

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

### 13 - Check contractor sample for SIMDQ ----

contractor.sample <- contractor.sample %>% 
  mutate(simdq = 0,
         simdq = ifelse(simd20rank < 1396, 1, simdq),
         simdq = ifelse(simd20rank >= 1396 & simd20rank < 2791, 2, simdq),
         simdq = ifelse(simd20rank >= 2791 & simd20rank < 4186, 3, simdq),
         simdq = ifelse(simd20rank >= 4186 & simd20rank < 5581, 4, simdq),
         simdq = ifelse(simd20rank >= 5581, 5, simdq))

contractor.sample.simdq <- contractor.sample %>% 
  group_by(laa, simdq) %>% 
  summarise(n = n(),
            .groups = 'drop') %>%
  group_by(laa) %>%
  mutate(percent = n/sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = simdq, 
              names_prefix = "sample_simdq_",
              values_from = percent,
              values_fill = list(percent = 0))
    
contractor.sample.previous <- contractor.sample.previous %>% 
  mutate(simdq = 0,
         simdq = ifelse(simd20rank < 1396, 1, simdq),
         simdq = ifelse(simd20rank >= 1396 & simd20rank < 2791, 2, simdq),
         simdq = ifelse(simd20rank >= 2791 & simd20rank < 4186, 3, simdq),
         simdq = ifelse(simd20rank >= 4186 & simd20rank < 5581, 4, simdq),
         simdq = ifelse(simd20rank >= 5581, 5, simdq))

contractor.sample.previous.simdq <- contractor.sample.previous %>% 
  group_by(laa, simdq) %>% 
  summarise(n = n(),
            .groups = 'drop') %>%
  group_by(laa) %>%
  mutate(percent = n/sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = simdq, 
              names_prefix = "previous_simdq_",
              values_from = percent,
              values_fill = list(percent = 0))

diff_simdq <- cbind(diff = contractor.sample.simdq[,-1] - contractor.sample.previous.simdq[,-1]) %>%
  rename_with(.fn = \(x)sub("diff.sample", "diff", x))
  
contractor.simdq.qa <- cbind(contractor.sample.simdq[,1],
                                     contractor.sample.simdq[,-1],
                                     contractor.sample.previous.simdq[,-1],
                                     diff_simdq[,-1])

### 14 - Check contractor sample for urbrur ----

contractor.la.urbrur <- contractor.sample %>% 
  group_by(laa, dz11_urbrur2020) %>% 
  summarise(n = n(),
            .groups = 'drop') %>%
  group_by(laa) %>%
  mutate(percent = n/sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = dz11_urbrur2020, 
              names_prefix = "sample_urbrur_",
              values_from = percent,
              values_fill = list(percent = 0))

contractor.previous.la.urbrur <- contractor.sample.previous %>% 
  group_by(laa, dz11_urbrur2020) %>% 
  summarise(n = n(),
            .groups = 'drop') %>%
  group_by(laa) %>%
  mutate(percent = n/sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = dz11_urbrur2020, 
              names_prefix = "previous_urbrur_",
              values_from = percent,
              values_fill = list(percent = 0))

diff_urbrur <- cbind(diff = contractor.la.urbrur[,-1] - contractor.previous.la.urbrur[,-1])%>%
  rename_with(.fn = \(x)sub("diff.sample", "diff", x))

contractor.urbrur.la.qa <- cbind(contractor.la.urbrur[,1],
                                     contractor.la.urbrur[,-1],
                                     contractor.previous.la.urbrur[,-1],
                                     diff_urbrur[,-1])


# Compare number and percentages of sampled addresses per urbrur 
# classification with previous year

contractor.urbrur <- contractor.sample %>% 
  group_by(dz11_urbrur2020) %>% 
  summarise(contractor_n = n()) %>%
  mutate(contractor_perc = contractor_n/sum(contractor_n)*100)

contractor.previous.urbrur <- contractor.sample.previous %>% 
  group_by(dz11_urbrur2020) %>% 
  summarise(previous_n = n()) %>%
  mutate(previous_perc = previous_n/sum(previous_n)*100)

contractor.urbrur.qa <- cbind(contractor.urbrur, contractor.previous.urbrur[,-1]) %>%
  mutate(diff = previous_perc - contractor_perc)

### *EXPORT* ----

### 15 - Export checks to excel file for manual inspection  ----

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

wb <- createWorkbook()

for(i in 1:length(qa)){
  
  sheet <- addWorksheet(wb, names(qa[i]))
  data <- qa[[i]]
  
  # loop through list of objects and write each in separate sheet
  
  writeData(wb, 
            sheet = sheet,
            x = data)
  
  
  # set column width to auto to ensure everything can be read
  
  setColWidths(wb, sheet, cols = 1:ncol(data), widths = "auto")
  
  
  # add conditional formatting to relevant columns
  
  if(any(grepl("^diff", colnames(data)))){
    
    diff <- grep("^diff", colnames(data))
    conditionalFormatting(wb = wb, sheet = sheet,
                        style = c("red", "green", "red"),
                        cols = diff, rows = (1:nrow(data)+1),
                        type = "colourScale",
                        rule = c(-2.5, 0, 2.5)
                        )
  }
  
  redstyle <- createStyle(bgFill = "#FF0000")
  greenstyle <- createStyle(bgFill = "#00FF00")
  
  if(any(grepl("^check", colnames(data)))){
    
    check <- grep("^check", colnames(data))
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = check, rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = " <= 1",
                          style = greenstyle)
    
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = check, rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = " > 1",
                          style = redstyle)
  }
    
    if(any(grepl("^overlap", colnames(data)))){
      
      overlap <- grep("^overlap", colnames(data))
      conditionalFormatting(wb = wb, sheet = sheet,
                            cols = overlap, rows = 2:(nrow(data)+1),
                            type = "expression",
                            rule = ' == "no"',
                            style = greenstyle)
      
      conditionalFormatting(wb = wb, sheet = sheet,
                            cols = overlap, rows = 2:(nrow(data)+1),
                            type = "expression",
                            rule = ' == "yes"',
                            style = redstyle)
    }
  
  udprn <- grep("^udprn", colnames(data))
  if(names(qa[i]) == "previously.sampled.udprn"){
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = udprn, 
                          rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = ' != 0',
                          style = redstyle)
  }
}

saveWorkbook(wb, file = paste0(scjs.path, 
                               "scjs.contractorsample.",
                               syear,
                               " - QA.xlsx"), overwrite = TRUE)


