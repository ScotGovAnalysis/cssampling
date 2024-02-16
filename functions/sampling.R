#' @title Draw stratified systematic sample
#' 
#' @description This function orders the sampling frame by la_code,
#' urban/rural classification, SIMD rank, postcode and address, and 
#' draws a stratified systematic sample in line with the previously
#' imported sample size file.
#' 
#' @param df Name of the data table which is the sample frame.
#' 
#' @param sample_size Name of the data table which includes the 
#' sample size.
#' 
#' @returns Data frame of sampled addresses.
#' 
#' @examples
#' sampling(df = shes.sframe, sample_size = shes.samplesize)

sampling <- function(df, sample_size){
  
  # Order sampling frame prior to drawing the sample
  df <- df[order(df$la_code,
                 df$dz11_urbrur2020,
                 df$simd20rank,
                 df$postcode,
                 df$print_address),]
  
  # Draw stratified systematic sample
  totalsample <- strata(data = df,
                        stratanames = c("la_code"),
                        size = sample_size$total_n, 
                        method = c("systematic"), 
                        pik = df$totalsize)
  
  totalsample <- getdata(df, totalsample) %>% 
    clean_names_modified()

  return(totalsample)
}
