#' Imports previously sampled and used UDPRNs
#' 
#' @param sampling_year Year for which the sample is taken.
#' 
#' @param filepath File path of folder which is to be searched.
#' 
#' @returns UPDRNs present in the contractor sample that were sampled
#' within the last four years. Data frame also includes the name of the file
#' which contains the previous sample.
#' 
#' @examples
#' delivered_udprn(syear, datashare.path)

delivered_udprn <- function(sampling_year, filepath){
  
  # Create variables with all survey names and four years prior to sampling year
  usedaddressyears <- paste0(c(sampling_year-4, sampling_year-3, 
                               sampling_year-2, sampling_year-1, 
                               sampling_year))
  usedaddressyears <- capture.output(cat(usedaddressyears, sep = "|"))
  surveys <- "(shs)|(shes)|(scjs)|(ssas)"
  
  # Identify file names which contain the term 'delivered'
  files_del <- list.files(path = filepath,
                          pattern = paste0(".*(", usedaddressyears,").*delivered.*\\.csv$"),
                          full.names = TRUE,
                          recursive = TRUE,
                          ignore.case = TRUE)
  files_del
  
  # Remove delivered but never used UDPRNs
  never_used <- files_del[grepl("never used", files_del, 
                                ignore.case = TRUE)]
  files_del <- files_del[!(files_del %in% never_used)]
  files_del
  
  # Import all delivered UDPRNs
  prev.samples <- lapply(files_del, import_multiple_files_csv)
  prev.samples <- do.call("rbind", prev.samples)
  
  # Check if any of the recently sampled UDPRNs has been previously delivered
  udprn.qa <- prev.samples[0 %in% contractor.sample$udprn]
  
  # Print warning if drawn sample includes previously sampled addresses
  {
    if (nrow(udprn.qa) != 0)
    {warning("Drawn sample includes previously sampled addresses")}
  }
  
  return(udprn.qa)
}


