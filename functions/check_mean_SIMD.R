#' Compares the mean SIMD in the sample frame to the mean SIMD in the sample
#' 
#' @param total.sample Drawn sample (contractor + reserve)
#' 
#' @param paf Cleaned postcode address file (i.e., sample frame)
#' 
#' @returns List with mean and median SIMD in the sample and sample
#' frame. The last column indicates whether the mean SIMD of the sample frame
#' lies between the lower and upper CIs of the mean SIMD of the sample.
#' 
#' @examples
#' check_mean_simd(total.sample = total.sample, paf = paf)

check_mean_simd <- function(total.sample, paf) {
  
  # Calculate SIMD statistics for sample
  sample.simd <- total.sample %>% 
    group_by(la) %>% 
    summarise(mean_sample = mean(simd20rank),
              median_sample = median(simd20rank),
              n_sample = n(),
              sd = sd(simd20rank)) %>%
    mutate(se = sd / sqrt(n_sample),
           lower.ci_sample = mean_sample - qt(1 - (0.05 / 2), n_sample - 1) * se,
           upper.ci_sample = mean_sample + qt(1 - (0.05 / 2), n_sample - 1) * se) %>%
    select(-c(sd, se))
  
  # calculate SIMD statistics for PAF
  paf.simd <- paf %>% 
    group_by(la) %>%
    summarise(mean_paf = weighted.mean(simd20rank, totalsize),
              median_paf = median(simd20rank),
              n_paf = n(),
              .groups = 'drop')
  
  # Merge SIMD statistics of PAF and sample
  simd.qa <- merge(sample.simd, paf.simd, 
                   by = "la") %>%
    mutate(nsampnpaf = n_sample / n_paf,
           overlap = ifelse(lower.ci_sample < mean_paf & upper.ci_sample > mean_paf,
                            "yes", "no"))
  
  # Print warning if there is overlap between sample CIs and PAF
  {
    if (any(simd.qa$overlap != "yes"))
    {stop("The SIMD statistic of at least one sampled address does not overlap with the SIMD of the PAF")}
    }
  
  return(list(sample.simd, paf.simd, simd.qa))
  
}
