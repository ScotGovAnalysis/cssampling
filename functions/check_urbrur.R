#' Compares the urban/rural distribution of sample and sample frame
#' 
#' @param df Sample frame
#' 
#' @returns Data frame with the urban rural distribution of sample and sample
#' frame in each LA. The last column shows the difference between 
#' sample and sample frame.
#' 
#' @examples
#' urbrur.la.qa(df = shs.frameandmatchedsample)

check_urbrur <- function(df){
  
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
    mutate(diff = selected_No - selected_Yes)
  
  # Print warning if diff is <-2.5 or >2.5
  {
    if (min(urbrur.la.qa$diff) < -2.5 | max(urbrur.la.qa$diff) > 2.5)
    {stop(paste0("In at least one local authority, the difference in urbrur percentage ",
                 "between the sampled and non-sampled addresses is greater than expected"))}
    }
  
  return(urbrur.la.qa)
  
}
