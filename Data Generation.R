### library ####
library(EffectLiteR)
library(restriktor)
library(tidyverse)

#### data generation ####

generateData <- function(N, hypothesis=0, small.effect=0){
  # Check for valid hypothesis input
  if (!hypothesis %in% c(0, 1)) {
    stop("hypothesis must be 0 (null) or 1 (alternative)")
  }
  
  # Define design grid: 2 levels of k, 3 levels of x
  design <- expand.grid(k = 0:1, x = 0:2)
  
  # Define probability vector for sampling design combinations
  probability_vector <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
  # Below is unbalanced Design:
  # probability_vector <- c(0.1, 0.2, 0.1, 0.1, 0.3, 0.2)
  
  ## Ensure minimum cell allocation to avoid empty cells
  num_cells <- nrow(design) # number of cells
  # 1 individuum per cell
  base_inds <- 1:num_cells # a vector of row indices: 1 2 3 4 5 6
  # sample the remaining individuals
  remaining_N <- N - num_cells
  remaining_inds <- sample(1:num_cells, size = remaining_N, replace = TRUE, prob = probability_vector)
  # combine
  ind <- c(base_inds, remaining_inds)
  
  
  # Assign rows of design to sampled individuals
  d <- design[ind, ]
  
  # Construct design-specific covariates
  d <- within(d, {
    Ix1 <- as.numeric(x == 1)  # Treatment as usual
    Ix2 <- as.numeric(x == 2)  # New treatment
    
    if (hypothesis == 1){
      # Directional / informative hypothesis: treatments differ
      if (small.effect == 0){
        z <- 1 + 0.5*k + 0.3*Ix1 + 0.4*Ix2 + rnorm(N,0,2)
      
        b0 <- (0.1+0.8*z)                                       
        b1 <- (0.3+0.5*k+0.2*z)                                
        b2 <- (0.3+0.5*k+0.2*z+0.4*k*z)      
        
        y <- b0 + b1*Ix1 + b2*Ix2 + rnorm(N,0,0.7)
      }
      if (small.effect==1){
        z <- 1 + 0.5*k + 0.3*Ix1 + 0.4*Ix2 + rnorm(N,0,2)
        b0 <- (0.1+0.8*z)
        # z and b0 affect all groups equal, no need to change
        b1 <- 0.15 + 0.3*k + 0.1*z   # smaller mean
        b2 <- 0.3 + 0.5*k + 0.2*z + 0.4*k*z  # keep bigger
        
        y <- b0 + b1*Ix1 + b2*Ix2 + rnorm(N, 0, 1)  # more noise than before
        
      }
    }
    
    if (hypothesis == 0){
      # Null hypothesis: all treatments equal
      b0 <- 0
      b1 <- 0
      b2 <- 0
      
      z <- 0.5 + 0.05*k + rnorm(N, 0, 1)
      y <- b0 + b1*Ix1 + b2*Ix2 + rnorm(N, 0, 1.75)
    }
  })

  # Return the generated data frame
  return(d)
}


#-----------------------------------------------------------------------------------------------------------------------------
generateData2 <- function(N = 100, hypothesis = 0) {
  # Check for valid hypothesis input
  if (!hypothesis %in% c(0, 1)) {
    stop("hypothesis must be 0 (null) or 1 (alternative)")
  }
  
  # Simulate independent variables
  x <- as.numeric(sample(c(0, 1, 2), N, replace = TRUE))
  k <- as.numeric(sample(c(1, 0), N, replace = TRUE))
  z <- rnorm(N, mean = 50, sd = 10)
  
  
  if (hypothesis == 0) {
    # Null hypothesis: outcome is unrelated to predictors
    y <- rnorm(N, mean = 50, sd = 10)
  } else {
    # Alternative hypothesis: outcome is affected by predictors
    # Example coefficients: x (β = 2 per level), experience (β = 3), z (β = 0.5)
    y <- 45 + 2 * x + 3 * k + 0.5 * z + rnorm(N, mean = 0, sd = 5)
  }
  
  # Return as data frame
  data.frame(
    x = x,
    k = k,
    z = z,
    y = y0
  )
}
