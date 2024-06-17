#' Checks the urb/rur distribution in the current and previous contractor
#' sample
#' 
#' @param sample Drawn contractor sample
#' 
#' @param previous.sample Contractor sample from the previous year
#' 
#' @returns List with the urb/rur distribution in the current and previous
#' sample. The first list item shows the urb/rur distribution in each LA.
#' The second list item shows th overall urb/rur distribution.
#' The last column in each data frame always shows the difference between the 
#' current and previous sample. 
#' 
#' @examples
#' cs_check_contractor_urbrur(sample = contractor.sample,
#'                         previous.sample = contractor.sample.previous)

cs_check_contractor_urbrur <- function(sample, previous.sample){
  
  contractor.la.urbrur <- cs_la_grouping(df = sample,
                                      grouping_variable = dz11_urbrur2020) 
  
  contractor.previous.la.urbrur <- cs_la_grouping(df = previous.sample,
                                               grouping_variable = dz11_urbrur2020)
  
  contractor.urbrur.la.qa <- cs_prev_cur_comp(current_df = contractor.la.urbrur,
                                           previous_df = contractor.previous.la.urbrur)
  
  # Print warning if diff is greater or lower than the threshold
  {
    if (any(contractor.urbrur.la.qa %>% 
            select(starts_with("diff")) < ifelse(survey == "shes",
                                                 -shes.urbrur.threshold,
                                                 -paf_sample.threshold) |
            contractor.urbrur.la.qa %>% 
            select(starts_with("diff")) > ifelse(survey == "shes",
                                                shes.urbrur.threshold,
                                                paf_sample.threshold)))
    {warning(paste0("For at least one local authority and ",
                    "urban rural classification, the difference between ",
                 "previous and current sample is greater or lower than expected"))}
  }
  
  # Compare number and percentages of sampled addresses per urbrur 
  # classification with previous year
  
  contractor.urbrur <- cs_calc_perc(sample, 
                                 dz11_urbrur2020)
  
  contractor.previous.urbrur <- cs_calc_perc(previous.sample, 
                                          dz11_urbrur2020)
  
  contractor.urbrur.qa <- cs_prev_cur_comp(contractor.urbrur, 
                                        contractor.previous.urbrur) %>%
    select(-diff.n)
  
  # Print warning if diff is greater or lower than threshold
  {
    if (any(contractor.urbrur.qa %>% select(starts_with("diff")) < -paf_sample.threshold |
            contractor.urbrur.qa %>% select(starts_with("diff")) > paf_sample.threshold))
    {warning(paste0("For at least one urban rural classification, the percentage of ",
                 "sampled addresses differs considerably from ",
                 "last year's sample"))}
    }
  
  return(list(contractor.urbrur.la.qa, contractor.urbrur.qa))
  
}
