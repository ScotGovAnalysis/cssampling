#########################################################################
# Name of file - 01_PAF.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Imports the postcode address file (PAF) and pre-processes it
# for the sampling. 

# More specifically, it removes postcodes that don't have a udprn, 
# grid reference or are a large postcode holder. It then creates a 
# multiple occupancy indicator and removes dead postcodes. After that,
# datazone information is added and the PAF is split into residential 
# and non-residential addresses. SHeS strata are added and the results
# are checked before exporting the cleaned data.

# Note: A new PAF file needs to be requested from NRS every 6-12 months. 
# The current script then needs to be re-run to update the sampling frame.

#########################################################################

# clear environment
rm(list = ls())

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message(title("Execute PAF script"))

### 1 - Import files ----

# Add message to inform user about progress
message(normal("Import data"))

# Code to import the postcode address file, clean names, remove columns 
# that aren't needed, convert "" to NA and clean datazone variable
rawpaf <-  read_csv(infilenm.path,
                    col_names = TRUE,
                    na = c("", "NA"),
                    col_select = c(Organisation, Property, Street, 
                                   Locality, Town, Postcode, PrintAddress,
                                   Multi_occupancy, CouncilArea, UDPRN,
                                   YCOORD, XCOORD, "2011Datazone", LACode, 
                                   UPRN, CouncilTaxBand)) %>%
  clean_names_modified() %>%
  mutate(datazone = substr(x2011datazone, 1, 9),
         udprn = as.numeric(udprn))

# Import datazone information and add indicator for SHeS year
dz_info <- haven::read_sas(dz.path) %>%
  clean_names_modified()

# Import dead postcode file
postcodes <- read.csv(pcd.path, header = TRUE, na = "") %>%
  clean_names_modified() %>%
  select(postcode, date_of_deletion)

# Import SHeS strata file
shes.strata <- read.csv(shes.strata.path, 
                        header = TRUE, na = "") %>%
  clean_names_modified()

### 2 - Postcode address file (PAF) ----

# Add message to inform user about progress
message(normal("Subset PAF"))

# Count addresses with missing udprn
nrow(rawpaf %>% filter(is.na(udprn) == TRUE))

# Count addresses with missing coordinates
nrow(rawpaf %>% filter(is.na(xcoord) == TRUE | is.na(ycoord) == TRUE))

# Count addresses with missing datazone information
nrow(rawpaf %>% filter(is.na(x2011datazone) == TRUE))

# Subset addresses with udprn, coordinates and datazone information
rawpaf <- rawpaf %>% filter(is.na(udprn) != TRUE,
                             is.na(xcoord) != TRUE | is.na(ycoord) != TRUE,
                             is.na(x2011datazone) != TRUE)
nrow(rawpaf)

### 3 - Multiple occupancy indicator  ----

# Add message to inform user about progress
message(normal("Create multiple occupancy indicator"))

# Create a dummy variable counting every line in the PAF and 
# select needed variables
rawmo <- rawpaf %>%
  mutate(var = 1) %>%
  select(udprn, var, uprn)

# Create multiocc variable
# Groups rawmo by udprn and var and counts observations
mo <- rawmo %>%
  group_by(udprn, var) %>%
  summarise(multiocc = n(),
            .groups = "drop")%>%
  select(-var)

# Remove duplicate udprns
rawpaf <- rawpaf %>% 
  arrange(desc(is.na(udprn)),
          desc(is.na(uprn))) %>% 
  distinct(udprn, .keep_all = TRUE)
nrow(rawpaf)

# Merge multiocc variable with unique udprn values
paf <- merge(x = rawpaf, y = mo,
             by = "udprn", all.x = TRUE) %>%
  select(-multi_occupancy)

### 4 - Remove dead postcodes  ----

# Add message to inform user about progress
message(normal("Remove dead postcodes from PAF"))

# separate postcodes and remove duplicates
postcodes <- postcodes %>%
  mutate(postcode2 = postcode) %>% 
  separate(postcode2, into = c("postcod1", "postcod2"), sep = " ") %>%
  arrange(desc(is.na(postcode)),
          desc(is.na(date_of_deletion))) %>%
  distinct(postcode, .keep_all = TRUE)

# Subset alive postcodes
alive <- postcodes %>% filter(is.na(date_of_deletion))
nrow(alive)

# Subset dead postcodes
dead <- postcodes %>% filter(!is.na(date_of_deletion))
nrow(dead)

# Identify postcodes which end with 4 characters
alive <- alive %>% 
  mutate(postcod3 = substr(postcod2, 1, 3)) %>%
  filter(postcod2 != postcod3)

# Merge alive postcodes with 4 ending characters and dead postcodes, 
# insert NAs and select needed variables
dead <- alive %>% 
  select(-postcod2) %>%
  rename_at('postcod3', ~'postcod2') %>%
  merge(x = dead,
        by = c("postcod1", "postcod2"), all.x = TRUE) %>% 
  mutate(date_of_deletion = ifelse(!is.na(postcode.y), NA, date_of_deletion.x),
         postcode = postcode.x) %>%
  select(postcod1, postcod2, postcode, date_of_deletion)

# Identify dead postcodes
dead <- dead %>% filter(!is.na(date_of_deletion))
nrow(dead)

# Split postcode in PAF, select needed variables and 
# merge with dead postcodes
# deadpconpaf contains all udprns that have a dead postcode
deadpconpaf <- paf %>%
  mutate(postcod1 = sapply(str_split(postcode, " "), 
                           function(x) x[1]),
         postcod2 = sapply(str_split(postcode, " "), 
                           function(x) x[2])) %>%
  select(postcod1, postcod2, print_address, udprn) %>%
  merge(y = dead, by = c("postcod1", "postcod2"))
nrow(deadpconpaf)

# Merges PAF with udprns known to have dead postcodes 
deadpconpaf1 <- merge(x = paf, y = deadpconpaf,
                      by = "udprn", all.y = TRUE)

# deadpconpaf1 should be empty
# stop if this isn't the case
{
  if (all(nrow(deadpconpaf1) == 0) == FALSE)
  {stop("PAF should not contain udprns known to have dead postcodes")}
}

# Merges PAF with udprns known to have dead postcodes and 
# saves a clean PAF which contains only udprns with live postcodes
clean_paf <- anti_join(x = paf, y = deadpconpaf,
                  by = "udprn") %>%
  rename_at('x2011datazone', ~'dz11')

# Merge PAF with datazone information required for sample selection
clean_paf <- clean_paf %>%
  left_join(dz_info, by = "dz11")

### 5 - Residential vs non-residential split  ----

# Code to split addresses into two datasets:
# 1. res is addresses that we can be confident are residential
# 2. nonres is addresses that do not have a multi occupancy indicator 
# but have an organisational name

# Add message to inform user about progress
message(normal("Split addresses into residential and non-residential"))

# Count non-residential addresses
nrow(clean_paf %>% 
  filter(multiocc == 1 & is.na(organisation) == FALSE))

# Subset residential addresses
res <- clean_paf %>% 
  filter(multiocc != 1 | is.na(organisation) ==  TRUE)
nrow(res)

# Removes addresses with "Unit" in the property variable 
# (with the exception of those containing "Flat") as they are 
# considered to be commercial addresses.
unit <- res %>% filter(grepl("Unit", property) & 
                         !grepl("Flat", property))
nrow(unit)

# Remove unit commercial addresses from residential addresses
residential <- anti_join(x = res, y = unit,
                         by = "udprn")
nrow(residential)

### 6 - SHeS strata  ----

# Add message to inform user about progress
message(normal("Add SHeS strata"))

# Code to add SHeS year sample data. 
residential <- shes.strata %>%
  mutate(shes_y1 = ifelse(shes_set == "A", 1, 0),
         shes_y2 = ifelse(shes_set == "B", 1, 0),
         shes_y3 = ifelse(shes_set == "C", 1, 0),
         shes_y4 = ifelse(shes_set == "D", 1, 0)) %>%
  right_join(dz_info) %>%
  right_join(residential)
nrow(residential)

# Remove observations with infrequent la_scode, la_code and la combination
pafaux <- residential %>% 
  filter(is.na(la_code) == FALSE) %>%
  group_by(la_scode, la_code, la) %>% 
  count() %>%
  filter(n > 1000)

# Merge residential with pafaux
paf_check <- residential %>% 
  left_join(pafaux)
nrow(paf_check)

# Harmonise la and la_code variables
final_paf <- paf_check %>%
  mutate(la_code = 0,
         la = ifelse(la_scode == "S12000033", "Aberdeen City", la),
         la = ifelse(la_scode == "S12000034", "Aberdeenshire", la),
         la = ifelse(la_scode == "S12000041", "Angus", la),
         la = ifelse(la_scode == "S12000035", "Argyll and Bute", la),
         la = ifelse(la_scode == "S12000005", "Clackmannanshire", la),
         la = ifelse(la_scode == "S12000006", "Dumfries and Galloway", la),
         la = ifelse(la_scode == "S12000042", "Dundee City", la),
         la = ifelse(la_scode == "S12000008", "East Ayrshire", la),
         la = ifelse(la_scode == "S12000045", "East Dunbartonshire", la),
         la = ifelse(la_scode == "S12000010", "East Lothian", la),
         la = ifelse(la_scode == "S12000011", "East Renfrewshire", la),
         la = ifelse(la_scode == "S12000036", "City of Edinburgh", la),
         la = ifelse(la_scode == "S12000013", "Na h-Eileanan Siar", la),
         la = ifelse(la_scode == "S12000014", "Falkirk", la),
         la = ifelse(la_scode == "S12000047", "Fife", la),
         la = ifelse(la_scode == "S12000049", "Glasgow City", la),
         la = ifelse(la_scode == "S12000017", "Highland", la),
         la = ifelse(la_scode == "S12000018", "Inverclyde", la),
         la = ifelse(la_scode == "S12000019", "Midlothian", la),
         la = ifelse(la_scode == "S12000020", "Moray", la),
         la = ifelse(la_scode == "S12000021", "North Ayrshire", la),
         la = ifelse(la_scode == "S12000050", "North Lanarkshire", la),
         la = ifelse(la_scode == "S12000023", "Orkney Islands", la),
         la = ifelse(la_scode == "S12000048", "Perth and Kinross", la),
         la = ifelse(la_scode == "S12000038", "Renfrewshire", la),
         la = ifelse(la_scode == "S12000026", "Scottish Borders", la),
         la = ifelse(la_scode == "S12000027", "Shetland Islands", la),
         la = ifelse(la_scode == "S12000028", "South Ayrshire", la),
         la = ifelse(la_scode == "S12000029", "South Lanarkshire", la),
         la = ifelse(la_scode == "S12000030", "Stirling", la),
         la = ifelse(la_scode == "S12000039", "West Dunbartonshire", la),
         la = ifelse(la_scode == "S12000040", "West Lothian", la),
         la_code = ifelse(la_scode == "S12000033", "100", la_code),
         la_code = ifelse(la_scode == "S12000034", "110", la_code),
         la_code = ifelse(la_scode == "S12000041", "120", la_code),
         la_code = ifelse(la_scode == "S12000035", "130", la_code),
         la_code = ifelse(la_scode == "S12000005", "150", la_code),
         la_code = ifelse(la_scode == "S12000006", "170", la_code),
         la_code = ifelse(la_scode == "S12000042", "180", la_code),
         la_code = ifelse(la_scode == "S12000008", "190", la_code),
         la_code = ifelse(la_scode == "S12000045", "200", la_code),
         la_code = ifelse(la_scode == "S12000010", "210", la_code),
         la_code = ifelse(la_scode == "S12000011", "220", la_code),
         la_code = ifelse(la_scode == "S12000036", "230", la_code),
         la_code = ifelse(la_scode == "S12000013", "235", la_code),
         la_code = ifelse(la_scode == "S12000014", "240", la_code),
         la_code = ifelse(la_scode == "S12000047", "250", la_code),
         la_code = ifelse(la_scode == "S12000049", "260", la_code),
         la_code = ifelse(la_scode == "S12000017", "270", la_code),
         la_code = ifelse(la_scode == "S12000018", "280", la_code),
         la_code = ifelse(la_scode == "S12000019", "290", la_code),
         la_code = ifelse(la_scode == "S12000020", "300", la_code),
         la_code = ifelse(la_scode == "S12000021", "310", la_code),
         la_code = ifelse(la_scode == "S12000050", "320", la_code),
         la_code = ifelse(la_scode == "S12000023", "330", la_code),
         la_code = ifelse(la_scode == "S12000048", "340", la_code),
         la_code = ifelse(la_scode == "S12000038", "350", la_code),
         la_code = ifelse(la_scode == "S12000026", "355", la_code),
         la_code = ifelse(la_scode == "S12000027", "360", la_code),
         la_code = ifelse(la_scode == "S12000028", "370", la_code),
         la_code = ifelse(la_scode == "S12000029", "380", la_code),
         la_code = ifelse(la_scode == "S12000030", "390", la_code),
         la_code = ifelse(la_scode == "S12000039", "395", la_code),
         la_code = ifelse(la_scode == "S12000040", "400", la_code))

# Check harmonisation was successful 
# (all local authorities should have more than 1000 addresses)
final_paf_check <- final_paf %>% group_by(la) %>% count()
{
  if (all(final_paf_check$n > 1000) == FALSE)
    {stop("At least one local authority has fewer than 1,000 addresses")}
}

### 7 - Export final PAF  ----

# Add message to inform user about progress
message(normal("Export final PAF"))

# Code to export final PAF into lookups folder
write_rds(
  final_paf,
  here("lookups", paste0(paf_v, "_final_paf.rds")),
  compress = "gz"
)

### END OF SCRIPT ####

# clear environment
rm(list=ls())
