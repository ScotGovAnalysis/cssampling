#' Calculate percentages of grouping variable
#' 
#' @param df Data frame which includes the grouping variable.
#' 
#' @param grouping_variable Variable which is used to group the data frame.
#' 
#' @returns Data frame with counts and percentages of the 
#' grouping_variable.
#' 
#' @examples
#' calc_perc(contractor.sample, dz11_urbrur2020)


calc_perc <- function(df, grouping_variable) {
  group <- enquo(grouping_variable)
  df %>% 
  group_by(!!group) %>% 
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)*100)
}
