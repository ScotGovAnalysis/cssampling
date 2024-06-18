#' Checks how many business and industrial addresses were sampled
#' 
#' @param sample Drawn contractor sample
#' 
#' @returns Error message if too many business or industrial addresses 
#' were sampled.
#' 
#' @examples
#' css_check_contractor_businesses(sample = contractor.sample)

css_check_contractor_businesses <- function(sample){
  
  contractor.business <- sample %>% 
    filter(grepl('Business', print_address))
  {
    if(nrow(contractor.business) > config$business.threshold)
    {warning("More than 10 business addresses are in the contractor sample.")}
    }
  
  contractor.indust <- sample %>% 
    filter(grepl('Industrial', print_address))
  {
    if(nrow(contractor.indust) > config$business.threshold)
    {warning("More than 10 industrial addresses are in the contractor sample.")}
    }
  
}
