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
#' stream_allocation(1, 4)

stream_allocation <- function(start, finish){
  num <- start:finish
  rand <- runif(finish-start+1)
  streams <- as_tibble(cbind(num, rand))
  streams <- streams[order(rand),]
  return(streams)
}
  