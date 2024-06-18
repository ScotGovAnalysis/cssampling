#' Compares the urban/rural distribution of sample and sample frame
#' 
#' @param df Sample frame
#' 
#' @returns Data frame with the urban rural distribution of sample and sample
#' frame in each LA. The last column shows the difference between 
#' sample and sample frame.
#' 
#' @examples
#' css_urbrur.la.qa(df = shs.frameandmatchedsample)

css_check_urbrur <- function(df){
  
  # Calculate urbrur percentage of sampled and non-sampled addresses
  # in each local authority
  urbrur.la.qa <- df %>%
    group_by(la, dz11_urbrur2020, selected) %>%
    summarise(n = n(),
              .groups = 'drop') %>%
    group_by(la, selected) %>%
    mutate(freq = n / sum(n) * 100) %>% 
    select(-n) %>%
    pivot_wider(names_from = selected, 
                names_prefix = "selected_",
                values_from = freq) %>%
    mutate(across(starts_with("selected"), ~replace(., is.na(.), 0)),
           diff = selected_No - selected_Yes) %>%
    select(-contains("_NA"))
  
  # Print warning if diff is too great
  {
    if (min(urbrur.la.qa$diff) < -config$paf_sample.threshold | max(urbrur.la.qa$diff) > config$paf_sample.threshold)
    {warning(paste0("In at least one local authority, the difference in urbrur percentage ",
                 "between the sampled and non-sampled addresses is greater than expected"))}
    }
  
  return(urbrur.la.qa)
  
}
