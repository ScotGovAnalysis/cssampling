#' Checks the stream distribution in the contractor sample
#' 
#' @param sample Drawn contractor sample
#' 
#' @param grouping_variable Variable that is used to group the sample
#' 
#' @param additional_grouping_variable Additional variable that may be used to
#' group the variable. This argument is optional.
#' 
#' @returns Data frame with stream distribution by the grouping variable.
#' The last column shows the difference between the lowest and the highest
#' stream occurrences.
#' 
#' @examples
#' check_stream(sample = contractor.sample,
#'              grouping_variable = la)

check_stream <- function(sample, grouping_variable, 
                         additional_grouping_variable = NULL){
  
  # defuse grouping_variable
  group <- rlang::enquo(grouping_variable)
  add.group <- rlang::enquo(additional_grouping_variable)
  
  # Checks allocation of sample across individual streams by LA
  # For each local authority, calculate the total and mean number of 
  # households per stream.
  # The check column indicates the difference between the minimum and 
  # the maximum values.
  contractor.stream.qa <- sample %>%
    group_by(across(c(!!group, !!add.group))) %>%
    count(stream) %>%
    mutate(max = max(n),
           min = min(n)) %>%
    ungroup %>%
    pivot_wider(id_cols = c(!!group, !!add.group, min, max), 
                names_from = stream, 
                values_from = n,
                values_fill = list(perc = 0)) %>%
    adorn_totals(c("col")) %>%
    mutate(mean = (Total-max-min)/12,
           check = max-min) %>%
    select(-c(max, min))
  
  # perform check if survey is SCJS
  if(survey == "scjs") {
    # check streams are equally distributed per local authority
    {
      if(!all(contractor.stream.qa$check %in% c(0:scjs.stream.threshold)))
      {warning("Streams are not equally distributed across local authorities.")}
    }
  }
  
  # perform check if survey is SHS
  if(survey == "shs") {
    # check streams are equally distributed per local authority
    {
      if(!all(contractor.stream.qa$check < shs.stream.threshold))
      {warning("Streams are not equally distributed across local authorities.")}
    }
  }
  
  return(contractor.stream.qa)
}
