#' @title Draw stratified systematic sample
#' 
#' @description This function orders the sampling frame by la_code,
#' urban/rural classification, SIMD rank, postcode and address, and 
#' draws a stratified systematic sample in line with the previously
#' imported sample size file.
#' 
#' @param df Name of the data table which is the sample frame.
#'
#' @param stratum Name of the stratum used for stratification.
#'
#' @param sample_size Name of the data table which includes the 
#' sample size.
#'
#' @param prob Vector with selection probabilities of all addresses in df.
#' 
#' @param control Name of variables the df is sorted by prior to sampling.
#' 
#' @returns Data frame of sampled addresses.
#' 
#' @examples
#' css_sampling(df = child.sframe, 
#'                   stratum = "la_code",
#'                   sample_size = child.samplesize$child_n,
#'                   prob = rep(1/nrow(child.sframe), times = nrow(child.sframe)),
#'                   control = shes.control))

css_sampling <- function(df, stratum, sample_size, prob, control){
  
  # Order sampling frame prior to drawing the sample
  order.var <- c(stratum, control) 
  df <- df %>% arrange(across({{ order.var }}))

  # reassign probability if function argument was a string
  # the previous steps reorders the data frame
  # this step ensures that this new order is reflected in prob
  # this step is only applicable if prob is a column of df
  # if all records have an equal selection probability
  # (e.g., prob = rep(1/nrow(child.sframe), times = nrow(child.sframe)), 
  # this step is skipped
  if(is.character(prob) == TRUE) {
    # remove addresses with selection probability of 0
    df <- df %>% filter(eval(rlang::parse_expr(prob)) != 0)
    prob <- df[[as.name(prob)]]
  }
  
  # Draw stratified systematic sample
  totalsample <- strata(data = df,
                        stratanames = stratum,
                        size = sample_size, 
                        method = c("systematic"), 
                        pik = prob)
  
  totalsample <- getdata(df, totalsample) %>% 
    css_clean_names_modified()
  
  return(totalsample)
}
