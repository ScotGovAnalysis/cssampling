#' Keep survey names as one word when cleaning variable names
#' 
#' @param data Data frame whose variable names are to be cleaned.
#' 
#' @returns Variable names are cleaned in accordance with the clean_names() 
#' function from the janitors package. Survey names are kept as one word.
#' For example, 'SHeS' becomes 'shes' rather than 's_he_s'.
#' 
#' @examples
#' css_clean_names_modified(x)

css_clean_names_modified <- function(data){
  clean_names(data, 
              replace = c("SHeS" = "shes",
                          "SheS" = "shes",
                          "SHS" = "shs",
                          "SCJS" = "scjs")) %>%
    rename(any_of(c(la = "laa")))
}
