#' Add SIMD quintiles to data frame
#' 
#' @param df Data frame containing SIMD rank information.
#' 
#' @returns Divides data frame into five parts based on SIMD ranks
#' - data is segregated into SIMD quintiles.
#' 
#' @examples
#' add_simdq(contractor.sample)
#' 
add_simdq <- function(df){
  df %>% 
    mutate(simdq = 0,
           simdq = ifelse(simd20rank < 1396, 1, simdq),
           simdq = ifelse(simd20rank >= 1396 & simd20rank < 2791, 2, simdq),
           simdq = ifelse(simd20rank >= 2791 & simd20rank < 4186, 3, simdq),
           simdq = ifelse(simd20rank >= 4186 & simd20rank < 5581, 4, simdq),
           simdq = ifelse(simd20rank >= 5581, 5, simdq))
}
