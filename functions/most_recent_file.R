#' Identify the most recent file
#' 
#' @param path Path to folder in which the files which are to be 
#' searched are located.
#' 
#' @param pattern Only file names matching the pattern will be returned. 
#' To return all files, use "".
#' 
#' @returns The file name of the most recent file in the path folder 
#' which matches the specified pattern.
#' 
#' @examples
#' most_recent_file(getwd(), "paf")
#' most_recent_file(here("lookups"), "usedaddresses")

most_recent_file <- function(path, pattern){
  list.files(path = path,
             pattern = pattern) %>% 
    sort() %>% 
    tail(1)
}
