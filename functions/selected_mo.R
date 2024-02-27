#' @title Select household in multiple occupancy addresses
#'
#' @description This function selects the household in multiple occupancy
#' addresses which is to be contacted by the fieldworkers.
#' 
#' @param df Data frame of sampled addresses.
#'
#' @returns A data frame with a new column 'selected_mo' which indicates 
#' which household is to be contacted.
#'
#' @examples
#' selected_mo(scjs.contractor.export)

selected_mo <- function(df){
  df %>%
    mutate(rand = (runif(nrow(df), 0, 1)),
           selected_mo = round(rand*multisize + 0.5, 0)) %>%
    select(-rand)
}
