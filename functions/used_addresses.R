#' @title Add metrics for active addresses
#'
#' @description This function creates an activeflag for active 
#' addresses, joins the PAF with SIMD rank data,
#' active and pactive
#' 
#' @param prev_samples Data frame which includes UDPRNs of 
#' addresses sampled within the last four years.
#' 
#' @param paf Data frame which is the cleaned postcode address 
#' file.
#'
#' @returns A tibble with all addresses, their active status,
#' SIMD rank, as well as the percentage and number of 
#' active addresses per data zone.
#'
#' @examples
#' used_addresses(prev_samples = usedaddresses, paf = clean_paf)

used_addresses <- function(prev_samples, paf){
  
  sframe <- prev_samples %>% 
    
    # Add flag for used addresses
    mutate(udprn = as.numeric(udprn),
           activeflag = 0) %>%
    
    # Merge used addresses with clean PAF
    right_join(paf, by = "udprn") %>%
    
    # Add flag for active addresses
    mutate(activeflag = ifelse(is.na(activeflag) == TRUE, 
                               1, activeflag)) %>%
    
    # Merge with SIMD ranks
    left_join(dz11_simd20) %>%
    
    # Compute percentage and number of active addresses
    group_by(dz11) %>% 
    mutate(pactive = mean(activeflag),
           total = length(activeflag)) %>%
    ungroup()

  return(sframe)
  
}
