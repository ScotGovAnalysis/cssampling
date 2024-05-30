#' Compares the SIMD distribution of contractor sample and sample frame
#' 
#' @param sample Drawn contractor sample
#' 
#' @param paf.simd Data frame with n and median and mean SIMD per LA
#' 
#' @returns Data frame with mean SIMD in contractor sample and sample frame
#' in each LA. THe last column indicates the difference between sample and 
#' sample frame.
#' 
#' @examples
#' check_contractor_simd(sample = contractor.sample, paf.simd = simd.qa[[2]],
#'                       grouping_variable = la)

check_contractor_simd <- function(sample, paf.simd, grouping_variable){
  
  # defuse grouping_variable
  group <- rlang::enquo(grouping_variable)
  
  contractor.simd.qa <- sample %>% 
    group_by(!!group) %>% 
    summarise(n = n(),
              mean_contractor = mean(simd20rank),
              .groups = 'drop')  %>%
    left_join(paf.simd, by = as_name(group)) %>%
    select(-c(median_paf, n_paf)) %>%
    mutate(diff = mean_contractor/mean_paf-1)
  
  # Print warning if diff is lower or greater than threshold
  {
    if (min(contractor.simd.qa$diff) < -paf_sample.threshold | 
        max(contractor.simd.qa$diff) > paf_sample.threshold)
    {warning(paste0("For at least one local authority, the mean SIMD difference ",
                 "between PAF and contractor sample is greater than expected"))}
    }
  
  return(contractor.simd.qa)
}
