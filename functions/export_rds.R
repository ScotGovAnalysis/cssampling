#' @title Export R object as RDS object.
#'
#' @description This function detects which survey the object 
#' relates to, determines the relevant file path and exports 
#' the object to an RDS object.
#'
#' @param object R object that is to be exported. The object name
#' must begin with the survey name and a dot (e.g., "shes.")
#'
#' @returns Rds file in output folder of survey for sampling year.
#'
#' @examples
#' css_export_rds(shs.totalsample)

css_export_rds <- function(object) {
  
  # get file path of output folder
  survey <- gsub("\\..*", "", as.name(substitute(object)))
  path <- eval(as.name(paste0(survey, ".path")))
  
  # write object to file path
  write_rds(
  object, 
  paste0(path,
         Sys.Date(),
         "_",
         substitute(object),
         ".",
         config$syear,
         ".rds"),
  compress = "gz")
} 
