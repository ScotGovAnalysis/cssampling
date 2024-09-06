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
  
  # add explanation to cover sheet of QA output
  cover <- c("business.addresses", paste0("This sheet shows all sampled business addresses. ",
                                 "The number of sampled business addresses should be ",
                                 "lower or equal to ",
                                 config$business.threshold,
                                 "."))
  
  # Confirm that the number of businesses is low
  # print warning if this isn't the case
  {
    if(nrow(business.qa) > config$business.threshold)
    {warning("More than 10 business addresses have been sampled (total sample).")}
    }
  
  return(list(cover, business.qa))
}
