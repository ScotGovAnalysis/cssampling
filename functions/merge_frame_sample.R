#' @title Merge sample frame with drawn sample
#'
#' @description This function merges the sample frame with 
#' the drawn sample and sorts it by local authority, urban/rural
#' classification, SIMD rank, postcode and address.
#'
#' @param sample_frame Name of the data table which is the 
#' sample frame.
#' 
#' @param totalsample Name of the data table which includes 
#' the drawn sample.
#'
#' @returns Data table with the merged information from the 
#' sample frame and the drawn sample.
#'
#' @examples
#' cs_merge_frame_sample(sample_frame = shs.sframe, 
#'                    totalsample = shs.totalsample)

cs_merge_frame_sample <- function(sample_frame, totalsample) {
  sample_frame %>%
  left_join(totalsample,
            by = join_by(udprn),
            suffix = c('', '.y')) %>%
    select(-contains('.y')) %>%
  arrange(la_code, dz11_urbrur2020, simd20rank, postcode, 
          print_address)
}