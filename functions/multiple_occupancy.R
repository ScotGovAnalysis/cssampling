#' @title Create multiple occupancy indicator
#'
#' @description This function selects relevant variables,
#' recodes and filters the multiocc variable, and
#' calculates the totalsize variable.
#'
#' @param sample_frame Name of the data frame which is the
#' sample frame.
#'
#' @returns Filtered data frame with multisize and totalsize 
#' variables.
#'
#' @examples
#' cs_multiple_occupancy(sample_frame = shes.sframe)

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


cs_multiple_occupancy <- function(sample_frame) {
  
  sframe <- sample_frame %>%
    
    # Drop all addresses where multiocc is greater than 50
    filter(multiocc <= 50) %>%
    
    # Select relevant variables
    select(any_of(c(
      "udprn",
      "organisation",
      "property",
      "street",
      "locality",
      "town",
      "postcode",
      "print_address",
      "xcoord",
      "ycoord",
      "datazone",
      "dz11",
      "la",
      "laa",
      "activeflag",
      "pactive",
      "la_code",
      "multiocc",
      "dz11_urbrur2020",
      "simd20rank",
      "average_simd20_rank",
      "dz11_urbrur2020_8fold",
      "shes_clustersize",
      "council_tax_band",
      "hb_code",
      "cluster21",
      "chp"))) %>%
    
    # Create multisize variable:
    # If multiocc is 1 or 2, then multisize 1.
    # Otherwise, multisize = multiocc.
    mutate(
      multisize = ifelse(multiocc %in% c(1, 2), 1, multiocc),
      
      # Create totalsize variable:
      # multisize is multiplied by the total number of
      # addresses divided by the total number of non-used 
      # addresses.
      # In the case of SHeS, the results are then multiplied by
      # clustersize.
      totalsize = if (exists('shes_clustersize')) 
        {multisize * (activeflag / pactive) * shes_clustersize} else
        {multisize * (activeflag / pactive)},
      la_code = as.numeric(la_code))
  
  return(sframe)
}
