#' Compares the SIMD distribution of contractor sample and sample frame
#' 
#' @param sample Drawn contractor sample
#' 
#' @param paf.simd Data frame with n and median and mean SIMD per LA
#' 
#' @returns Data frame with mean SIMD in contractor sample and sample frame
#' in each LA. THe last column indicates the difference between sample and 
#' sample frame.
#' 
#' @examples
#' css_check_contractor_simd(sample = contractor.sample, paf.simd = simd.qa[[3]],
#'                       grouping_variable = la)

css_check_contractor_simd <- function(sample, paf.simd, grouping_variable){
  
  # defuse grouping_variable
  group <- rlang::enquo(grouping_variable)
  
  contractor.simd.qa <- sample %>% 
    group_by(!!group) %>% 
    summarise(n = n(),
              mean_contractor = mean(simd20rank),
              .groups = 'drop')  %>%
    left_join(paf.simd, by = as_name(group)) %>%
    select(-c(median_paf, n_paf)) %>%
    mutate(diff = mean_contractor/mean_paf-1)
  
  # add explanation to cover sheet of QA output
  if(as_label(group) == "la")
  cover <- c("contractor.simd.la", paste0("This sheet compares the mean SIMD by LA ",
                                 "in the drawn sample with the sampling frame. ",
                                 "The difference for each LA ",
                                 "between sample and PAF should be between ",
                                 -config$paf_sample.threshold,
                                 " and ",
                                 config$paf_sample.threshold,
                                 "."))
  
  if(as_label(group) == "hb_code")
    cover <- c("contractor.simd.hb", paste0("This sheet compares the mean SIMD by Health Board ",
                                            "in the drawn sample with the sampling frame. ",
                                            "The difference for each Health Board ",
                                            "between sample and PAF should be between ",
                                            -config$paf_sample.threshold,
                                            " and ",
                                            config$paf_sample.threshold,
                                            "."))
  
  # Print warning if diff is lower or greater than threshold
  {
    if (min(contractor.simd.qa$diff) < -config$paf_sample.threshold | 
        max(contractor.simd.qa$diff) > config$paf_sample.threshold)
    {warning(paste0("For at least one local authority, the mean SIMD difference ",
                 "between PAF and contractor sample is greater than expected"))}
    }
  
  return(list(cover, contractor.simd.qa))
}
