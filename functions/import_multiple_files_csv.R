#' Imports multiple csv files and retains filename in new column
#' 
#' @param filenames Filenames of all addresses sampled in the last four years.
#' 
#' @returns Imports the udprn of previously sampled addresses 
#' and retains their filename in a new column to allow further inspection.
#' 
#' @examples
#' css_import_multiple_files_csv(files_del)

css_import_multiple_files_csv <- function(filenames){
  df <- read.csv(filenames) %>%
    clean_names() %>%
    select(c("udprn"))
  df$filename <- filenames
  return(df)
}
