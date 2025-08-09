empirical_detection_rate <- function(arr) {
  # Get dimension names
  dims <- dimnames(arr)
  
  # result matrix
  result <- matrix(0, nrow = dim(arr)[3], ncol = dim(arr)[2])
  rownames(result) <- dims[[3]]  # rows correspond to the conditions (3rd dimension)
  colnames(result) <- dims[[2]]  # columns correspond to the sample sizes (2nd dimension)
  
  
  # Loop over each condition (3rd dimension)
  for (k in 1:dim(arr)[3]) {
    result[k, ] <- colMeans(arr[,,k])
  }
  
  return(result)
}
