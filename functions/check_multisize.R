#' Compares the multisize distribution of sample and sample frame
#' 
#' @param sample Drawn sample (contractor + reserve)
#' 
#' @param paf Sample frame
#' 
#' @returns Data frame with number and percentage of multisize addresses
#' in drawn sample and sample frame. The last column indicated the difference
#' between the two.
#' 
#' @examples
#' cs_check_multisize(sample = total.sample, paf = paf)

cs_check_multisize <- function(sample, paf){
  
  # Check multi occupancy size for sample and PAF 
  # Sample %s should be similar to PAF
  
  contractor.multisize <- sample %>% 
    group_by(multisize) %>%
    summarise(contractor_n = n()) %>%
    mutate(contractor_perc = contractor_n/sum(contractor_n)*100)
  
  total.multisize <- paf %>% 
    cs_calc_perc(grouping_variable = multisize) %>%
    group_by(multisize) %>%
    summarise(paf_n = n * multisize,
              paf_perc =  100 * paf_n/nrow(paf)) %>%
    mutate(cum_sum = cumsum(paf_n),
           cum_perc = cumsum(paf_perc))
  
  multisize.qa <- full_join(contractor.multisize[,1:3], 
                        total.multisize[,1:3],
                        by = "multisize") %>%
    arrange(multisize) %>%
    replace(is.na(.), 0) %>%
    mutate(diff = contractor_perc - paf_perc)
  
  # Print warning if diff is greater or lower than threshold
  {
    if (min(multisize.qa$diff) < -paf_sample.threshold | 
        max(multisize.qa$diff) > paf_sample.threshold)
    {warning(paste0("For at least one multisize category, ",
                 "the difference between PAF % and contractor sample %",
                 "is greater than expected"))}
    }
  
  return(multisize.qa)
}
