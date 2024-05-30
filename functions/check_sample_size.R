#' Checks if the drawn sample meets the sample size requirements
#' 
#' @param df Data frame with drawn addresses
#' 
#' @param sample.size Data frame with pre-agreed sample sizes
#' 
#' @returns Data frame with number of drawn samples per LA, 
#' sample size requirements per LA and difference between the two
#' 
#' @examples
#' check_sample_size(df = contractor.sample, sample.size = sample.size)

check_sample_size <- function(df, sample.size){
  
  # Compare sample size requirements with drawn sample
  contractor.sample.size.check <- contractor.sample %>% 
    group_by(la_code) %>% 
    summarise(drawn_n = n()) %>%
    left_join(sample.size, by = "la_code") %>%
    select(la_code, drawn_n, contractor_n) %>%
    mutate(diff = contractor_n - drawn_n)
  
  # Print warning if diff is not 0
  {
    if (any(contractor.sample.size.check$diff != 0))
    {stop("Drawn sample does not meet sample size requirements")}
    }
  
  return(contractor.sample.size.check)
  
}
