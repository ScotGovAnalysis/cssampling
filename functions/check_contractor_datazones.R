#' Checks the data zone distribution in the contractor sample
#' 
#' @param sample Drawn contractor sample
#' 
#' @param dz Data frame with data zone and local authority information
#' 
#' @param hh_estimates Data frame with the number of occupied dwellings 
#' in each data zone
#' 
#' @returns Data frame with the data zone distribution in the sample and
#' across the occupied dwellings. The last column indicates the difference
#' between the two.
#' 
#' @examples
#' css_check_contractor_datazones(sample = contractor.sample,
#'                            dz = dz_info,
#'                            hh_estimates = hh.est.dz)

css_check_contractor_datazones <- function(sample, dz, hh.estimates){
  
  contractor.datazone.qa <- sample %>% 
    
    # group by data zone
    group_by(datazone) %>%
    
    # count number of addresses
    summarise(n_sample = n()) %>%
    ungroup() %>%
    
    # add local authority information
    left_join(dz,
          by = c("datazone" = "dz11")) %>%
    
    # add occupied dwellings
    left_join(hh.estimates, 
          by = "datazone")%>% 
    
    # group by local authority
    group_by(la) %>%
    
    # calculate data zone distribution
    mutate(perc_hh = occupied_dwellings / sum(occupied_dwellings),
           perc_sample = n_sample / sum(n_sample),
           diff = perc_hh - perc_sample) %>%
    ungroup()
  
  # Print warning if diff is lower or greater than threshold
  {
    if (min(contractor.datazone.qa$diff) < -config$paf_sample.threshold | 
        max(contractor.datazone.qa$diff) > config$paf_sample.threshold)
    {stop(paste0("For at least one datazone, the percentage of occupied dwellings ",
                 "in the sample is greater or lower than expected"))}
    }
  
  return(contractor.datazone.qa)
}
