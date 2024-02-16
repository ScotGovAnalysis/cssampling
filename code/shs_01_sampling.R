#########################################################################
# Name of file - shs_01_sampling.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Imports the cleaned postcode address file, 
# removes previously sampled addresses, calculates selection probabilities
# and samples the addresses for the Scottish Household Survey (SHS).

#########################################################################

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("code", "00_setup.R"))

### 1 - Import files ---- 

# Identify most recent used addresses file
recent_usedaddresses <- most_recent_file(path = here("lookups"), 
                                         pattern = "usedaddresses")

# Import used addresses
usedaddresses <- read_rds(paste0(here("lookups"), "/", recent_usedaddresses))

# Identify most recent cleaned PAF
recent_paf <- most_recent_file(path = here("lookups"), pattern = "paf")

# Import cleaned PAF
clean_paf <- read_rds(paste0(here("lookups", "/", recent_paf)))

# Import SIMD ranks for datazones
dz11_simd20 <- haven::read_sas(dz_simd.path) %>%
  clean_names_modified()

# Import sample size file
shs.samplesize <- read.csv(shs.samplesize.path, 
                                    header = TRUE, na = "") %>%
  clean_names_modified()

### 2 - Used addresses ---- 

# Add flag for used addresses
usedaddresses <- usedaddresses %>% 
  mutate(udprn = as.numeric(udprn),
         activeflag = 0)

# Merge used addresses with clean PAF, update activeflag and
# merge with SIMD ranks
shs.sframe <- clean_paf %>% 
  left_join(usedaddresses, by = "udprn") %>%
  mutate(activeflag = ifelse(is.na(activeflag) == TRUE, 
                             1, activeflag)) %>%
  left_join(dz11_simd20)

# Check if all addresses have an SIMD rank
nrow(shs.sframe %>% filter(is.na(simd20rank) == TRUE)) == 0

# Compute percentage and number of active addresses
usedsize <- shs.sframe %>% 
  group_by(dz11) %>% 
  summarise(pactive = mean(activeflag),
            total = length(activeflag))

# Merge statistics with sampling frame
shs.sframe <- shs.sframe %>%
  left_join(usedsize, by = "dz11")

### 3 - Multiple occupancy  ---- 

# For addresses on the PAF with a multiple occupancy (MO) indicator 
# greater than 2, the size variable is set as the MO number.  
# For example, for an address with an MO of 5 (i.e. the address has 
# five households), the size variable will be set to five.

# Compared to addresses with an MO of one, the address with an MO 
# of five will be five times more likely to be in the sample. 
# If the address is then selected as part of the sample, the 
# interviewer will select one household at random (probability 
# of 1/5).  So each household at the address has a probability of 
# selection of 5 x 1/5 = 1.

# Any addresses with an MO of 2 are allocated a size of one.  
# Addresses with an MO of two are often a shop with a flat above so 
# only a single household. For cases where the interviewer 
# discovers two households, they will either interview both households
# or randomly select one household which will be given an adjusted 
# weight during weighting.

# The size variable is used for sampling proportionate to size.  
# The selection procedure uses the size variable as a convenient way to 
# ensure an equal probability sample of households 
# (for multiple occupancy and used addresses).

# Each element of the size variable is calculated separately and then 
# multiplied to get a total size variable.

# As at least one of the surveys will have a clustered design 
# (e.g., SHeS), there must be a size variable introduced to 
# counteract the effect of clustering on the sampling frame.  

# If the SHeS clustered in a quarter of Glasgow in Year 1, 
# then in Year 2 the addresses selected as part of the SHeS
# would be removed from the sampling frame.  When the SHS
# selected an unclustered sample across Glasgow in Year 2, 
# addresses in the quarter of Glasgow that was included in the 
# SHeS in Year 1 would have a lower probability than those 
# address in the three quarters of Glasgow that weren't included in 
# the SHeS in Year 1. Therefore the SHS would not have an equal 
# probability sample in Glasgow and would be implicitly clustered.

# To counteract this, a size variable can be calculated that accounts for
# the number of used addresses in each area. The used geography 
# must be common to all three surveys, so the obvious choice is 
# data zone (which will be the clusters for the SCJS).  The SHeS 
# will use Intermediate Geographies which are constructed from 
# data zones. To construct a size for each data zone, the total 
# number of addresses are divided by the total number of non-used 
# addresses.

# Select relevant variables, recode and filter multiocc variable,
# and calculate totalsize variable
shs.sframe <- shs.sframe %>%
  select(udprn, organisation, property, street, locality, town, 
         postcode, print_address, xcoord, ycoord, datazone, laa, 
         activeflag, pactive, la_code, multiocc, dz11_urbrur2020, 
         simd20rank, dz11_urbrur2020_8fold) %>%
  mutate(multisize = ifelse(multiocc %in% c(1, 2), 1, multiocc),
         totalsize = multisize * (activeflag/pactive),
         la_code = as.numeric(la_code)) %>%
  filter(multiocc <= 50)

# Number of addresses in sampling frame
nrow(shs.sframe)

### 4 - Sampling ---- 

# Order sampling frame prior to drawing the sample
shs.sframe <- shs.sframe[order(shs.sframe$la_code,
                                 shs.sframe$dz11_urbrur2020,
                                 shs.sframe$simd20rank,
                                 shs.sframe$postcode,
                                 shs.sframe$print_address),]

# Draw sample
# As the selection probability of some addresses is zero,
# a warning message will be displayed when executing the code below.
# This is to be expected and nothing to be concerned about.
shs.totalsample <- strata(data = shs.sframe, 
                                   stratanames = c("la_code"), 
                                   size = shs.samplesize$total_n, 
                                   method = c("systematic"), 
                                   pik = shs.sframe$totalsize)

shs.totalsample <- getdata(shs.sframe, shs.totalsample) %>% 
  clean_names_modified()

# Merge sampling frame and drawn sample and sort data frame
shs.frameandmatchedsample <- shs.sframe %>%
  left_join(shs.totalsample) %>%
  arrange(la_code, dz11_urbrur2020, simd20rank, postcode, 
          print_address)

### 5 - Export sample  ----

# Code to export sampled addresses into output folder

write_rds(
  shs.totalsample, 
  paste0(shs.path, 
         "shs.totalsample.",
         syear,
         ".rds"),
  compress = "gz"
)

write_rds(
  shs.samplesize, 
  paste0(shs.path,
         "shs.samplesize.",
         syear,
         ".rds"),
  compress = "gz"
)

write_rds(
  shs.frameandmatchedsample, 
  paste0(shs.path,
         "shs.frameandmatchedsample.",
         syear,
         ".rds"),
  compress = "gz"
)



