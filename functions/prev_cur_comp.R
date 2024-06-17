#' Compares distributions of current and previous samples.
#' 
#' @param current_df Current contractor sample.
#' 
#' @param previous_df Last year's contractor sample.
#' 
#' @returns A data frame which includes the distribution of a variable in
#' the current contractor sample and the previous year's contractor sample.
#' The diff column shows the difference between both samples.
#' 
#' @examples
#' cs_prev_cur_comp(contractor.la.urbrur, contractor.previous.la.urbrur)

cs_prev_cur_comp <- function(current_df, previous_df) {
  
  # calculate different between current and previous samples
  diff <- cbind(diff = current_df[,-1] - previous_df[,-1]) %>%
  rename_with(.fn = \(x)sub("diff.sample", "diff", x))
  
  # add prefix to column names to identify samples
  colnames(current_df)[-1] <- paste0("contractor_", colnames(current_df)[-1])
  colnames(previous_df)[-1] <- paste0("previous_", colnames(previous_df)[-1])
  
  # combine data frames into one
  qa <- cbind(current_df, previous_df[,-1], diff)
  
  return(qa)
}
