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
    
    # transform property column to character column to ensure that
    # Excel doesn't convert flat addresses to dates;
    mutate(property = as.character(property)) %>%
    
    # select relevant variables
    select(any_of(c(
      "udprn", "organisation", "property", "street"," locality", 
      "town", "postcode", "print_address", "datazone", "dz11", 
      "xcoord", "ycoord", "health_board", "hb_code", "la", 
      "la_code", "cluster21", "multisize", "simd20rank", "dz11_urbrur2020", 
      "council_tax_band",
      "houseconditionflag", "sample_type", "core", "core_bio", "child_boost",
      "hb_boost"))) %>%
    
    # sort data frame
    # if the variable 'houseconditionflag' exists, use it as sorting variable
    # otherwise, skip 'houseconditionflag'
  arrange(ifelse("houseconditionflag" %in% names(df), houseconditionflag, ""),
          la_code, dz11_urbrur2020, simd20rank, 
          postcode, print_address)
}
