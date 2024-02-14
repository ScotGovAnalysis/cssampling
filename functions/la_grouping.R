#' Creates summary statistics for a grouping variable by local authority
#' 
#' @param df Data frame.
#' 
#' @param grouping_variable Variable which is used to group the data frame
#' 
#' @returns Groups data frame by local authority and grouping variable, 
#' counts the number of observations in each group, calculates the 
#' distribution of the grouping variable in each local authority and 
#' transposes the output.
#' 
#' @examples
#' la_grouping(contractor.sample, dz11_urbrur2020)

la_grouping <- function(df, grouping_variable) {
  
  # defuse grouping_variable
  group <- rlang::enquo(grouping_variable)
  
  # get df name
  df_name <- deparse(substitute(df))
  
  # Group by laa and grouping_variable, count the number of 
  # observations in each group, calculate the distribution of 
  # the grouping variable in each local authority and 
  # transpose the output
  df %>% 
  group_by(laa, !!!group) %>% 
  summarise(n = n(),
            .groups = 'drop') %>%
  group_by(laa) %>%
  mutate(percent = n/sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = !!group, 
              names_prefix = paste0(rlang::as_name(group), 
                                    "_"),
              values_from = percent,
              values_fill = list(percent = 0))
}

