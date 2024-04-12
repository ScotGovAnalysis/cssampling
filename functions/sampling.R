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
#' @param prob Vector with selection probabilites of all addresses in df.
#' 
#' @param control Name of variables the df is sorted by prior to sampling.
#' 
#' @returns Data frame of sampled addresses.
#' 
#' @examples
#' sampling(sampling(df = child.sframe, 
#'                   stratum = "la_code",
#'                   sample_size = child.samplesize$child_n,
#'                   prob = rep(1/nrow(child.sframe), times = nrow(child.sframe)),
#'                   control = c))

sampling <- function(df, stratum, sample_size, prob, control){
  
  # Order sampling frame prior to drawing the sample
  order.var <- c(stratum, control) 
  df <- df %>% arrange(across({{ order.var }}))
  
  # reassign probability if argument was a string
  # the previous steps reorders the data frame
  # this step ensures that this new order is reflected in prob
  if(is.character(prob) == TRUE) {
    # remove addresses with slection probability of 0
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
    clean_names_modified()
  
  return(totalsample)
}
