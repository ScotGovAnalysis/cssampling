#' Compares the urban/rural distribution of sample and sample frame
#' 
#' @param Sample Drawn sample (contractor + reserve)
#' 
#' @returns Data frame with all sampled postcodes. The second column indicates
#' how often a postcode was sampled. The third columns shows if the postcode
#' is included in the contractor or the reserve sample.
#' 
#' @examples
#' css_check_postcodes(total.sample)

css_check_postcodes <- function(sample){
  
  # Add column indicating whether an address is part of the contractor sample 
  # or reserve sample
  pcode <- sample %>% 
    count(postcode) %>% 
    arrange(desc(n))
  
  # Check if any postcode has been sampled more than the threshold and if so
  # print warning
  {
    if (head(pcode$n, 1) > ifelse(survey == "shes", 
                                  config$shes.postcode.threshold,
                                  config$postcode.threshold))
    {warning(paste0("At least one postcode has been sampled more than anticipated.",
                    " Check Excel file to confirm these are all small postcodes."))}
    }

  return(pcode)
  
}
