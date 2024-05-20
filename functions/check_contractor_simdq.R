#' Checks the SIMDQ distribution in the current and previous contractor
#' sample
#' 
#' @param sample Drawn contractor sample
#' 
#' @param previous.sample Contractor sample from the previous year
#' 
#' @returns Data frame with the SIMDQ distribution in the current and previous
#' sample. The last columns show the difference between the current and 
#' previous sample.
#' 
#' @examples
#' check_contractor_simdq(sample = contractor.sample,
#'                        previous.sample = contractor.sample.previous)

check_contractor_simdq <- function(sample, previous.sample){
  
  add_simdq <- function(df){
    df %>% 
      mutate(simdq = 0,
             simdq = ifelse(simd20rank < 1396, 1, simdq),
             simdq = ifelse(simd20rank >= 1396 & simd20rank < 2791, 2, simdq),
             simdq = ifelse(simd20rank >= 2791 & simd20rank < 4186, 3, simdq),
             simdq = ifelse(simd20rank >= 4186 & simd20rank < 5581, 4, simdq),
             simdq = ifelse(simd20rank >= 5581, 5, simdq))
  }
  
  contractor.la.simdq <- sample %>%
    add_simdq() %>%
    la_grouping(simdq)
  
  contractor.previous.la.simdq <- previous.sample %>%
    add_simdq() %>%
    la_grouping(simdq)
  
  contractor.simdq.qa <- prev_cur_comp(current_df = contractor.la.simdq,
                                       previous_df = contractor.previous.la.simdq)
  
  # Print warning if diff is lower or greater than threshold
  {
    if (any(contractor.simdq.qa %>% 
            select(starts_with("diff")) < ifelse(survey == "shes",
                                                 -shes.simdq.threshold,
                                                 -simdq.threshold) |
            contractor.simdq.qa %>% 
            select(starts_with("diff")) > ifelse(survey == "shes",
                                                 shes.simdq.threshold,
                                                 simdq.threshold) ))
    {warning(paste0("For at least one local authority and SIMDQ rank,",
                    "the difference between previous and current sample is greater than expected"))}
  }
  
  return(contractor.simdq.qa)
}
