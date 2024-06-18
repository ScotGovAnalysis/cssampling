#' Identify most recent file based on date in filename
#' 
#' @param path Path to folder in which files are to be 
#' searched.
#' 
#' @param pattern Only file names matching the pattern will be returned. 
#' To return all files, use "".
#' 
#' @returns The file name of the most recent file in the path folder 
#' which matches the specified pattern. Filenames must include a date 
#' for the function to work properly.
#' 
#' @examples
#' css_most_recent_file(getwd(), "paf")
#' css_most_recent_file(here("lookups"), "usedaddresses")

css_most_recent_file <- function(path, pattern){
  list.files(path = path,
             pattern = pattern) %>% 
    sort() %>% 
    tail(1)
}
