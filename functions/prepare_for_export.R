#' @title Prepares post-processed sample for export
#'
#' @description This function transforms the property column to a 
#' character column, selects the required variables and sorts it by 
#' (houseconditionflag), local authority, urban rural classification,
#' SIMD rank, postcode and address.
#' 
#' @param df Post-processed data frame of sampled addresses.
#'
#' @returns A cleaned data frame ready to be exported to a csv file.
#'
#' @examples
#' prepare_for_export(scjs.contractor.export)

prepare_for_export <- function(df){
  df %>%
    mutate(property = as.character(property)) %>%
    select(any_of(c(
      "udprn", "organisation", "property", "street"," locality", 
      "town", "postcode", "print_address", "datazone", "dz11", 
      "xcoord", "ycoord", "laa", 
      "la_code", "multisize", "simd20rank", "dz11_urbrur2020", 
      "council_tax_band",
      "houseconditionflag"))) %>%
    arrange(if("houseconditionflag" %in% names(df)){houseconditionflag},
            la_code, dz11_urbrur2020, simd20rank, 
            postcode, print_address)
}
