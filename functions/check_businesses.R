#' Checks the number of business addresses in the sample
#' 
#' @param Sample Drawn sample (contractor + reserve)
#' 
#' @returns Data frame with sampled business addresses
#' 
#' @examples
#' css_check_businesses(sample = total.sample)

css_check_businesses <- function(sample){

# Inspect business addresses
business.qa <- sample %>% 
  filter(organisation != "") %>% 
  count(organisation)

# Confirm that the number of businesses is low
# print warning if this isn't the case
{
  if(nrow(business.qa) > config$business.threshold)
  {warning("More than 10 business addresses have been sampled (total sample).")}
  }

return (business.qa)
}
