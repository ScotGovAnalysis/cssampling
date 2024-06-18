#' @title Allocate streams to sampled addresses
#'
#' @description This function randomly allocates streams to the 
#' sampled addresses.
#' 
#' @param start First number of stream.
#' 
#' @param finish Last number of stream.
#'
#' @returns A data frame with randomly ordered streams.
#'
#' @examples
#' css_stream_allocation(1, 4)

css_stream_allocation <- function(start, finish){
  
  # create sequence of number from 'start' to 'finish'
  # e.g., if start = 1 and finish = 4, then 1 2 3 4 will be created
  num <- start:finish
  
  # generate random numbers
  rand <- runif(finish-start+1)
  
  # create data frame with sequence and random numbers
  streams <- as_tibble(cbind(num, rand))
  
  # order data frame by random number 
  #(this ensures that the streams are randomly ordered)
  streams <- streams[order(rand),]
  
  return(streams)
}
  
