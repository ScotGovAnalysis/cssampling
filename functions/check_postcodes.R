#' Compares the urban/rural distribution of sample and sample frame
#' 
#' @param Sample Drawn sample (contractor + reserve)
#' 
#' @returns Data frame with all sampled postcodes. The second column indicates
#' how often a postcode was sampled. The third columns shows if the postcode
#' is included in the contractor or the reserve sample.
#' 
#' @examples
#' check_postcodes(total.sample)

check_postcodes <- function(sample){
  
  # Add column indicating whether an address is part of the contractor sample 
  # or reserve sample
  pcode <- sample %>% 
    count(postcode) %>% 
    arrange(n) %>%
    mutate(allocation = ifelse(postcode %in% contractor.sample$postcode, 
                               "contractor", "reserve"))
  
  # Check if any postcode has been sampled more than once and if so
  # print warning
  {
    if (head(pcode$n, 1) > 5)
    {stop("At least one postcode has been sampled more than 5 times.")}
    }
  {
    if (tail(pcode$n, 1) > 5)
    {stop("At least one postcode has been sampled more than 5 times.")}
  }
  
  return(pcode)
  
}
