bain_preference <- function(bain_obj, true_hypothesis, cutoff="regular"){
  # catch some errors I encountered with bain objects
  
  #if (is.null(bain_obj) || is.null(bain_obj$fit) || length(bain_obj$fit$PMPc) == 0) {
  #  message("Warning: bain_obj is invalid, returning NA")
  #  print(bain_obj)
  #  return(as.numeric(NA))
  #}
  
################################################################################
# General settings
################################################################################
  threshold <- 0.95
  
  # get all PMPc´s of all hypotheses
  pmpc <- as.numeric(bain_obj$fit$PMPc)
  
################################################################################
# Give names to hypotheses
################################################################################
  if (is.null(names(bain_obj$fit$PMPc))) {
    names(pmpc) <- paste0("H", seq_along(pmpc))
    names(pmpc)[length(pmpc)] <- "Hc"  # last one is complement
  } else {
    names(pmpc) <- names(bain_obj$fit$PMPc)
  }

################################################################################
# Matching the hypotheses names to their index
################################################################################

  if (is.character(true_hypothesis)) { #if hypothesis is a character
    # give true index index of where true_hypothesis matches the name of a PMPc
    true_index <- match(true_hypothesis, names(pmpc))
    if (is.na(true_index)) {
      stop("true_hypothesis (character) not found in PMP names")
    }
  }
  else if (is.numeric(true_hypothesis)) {
    if (true_hypothesis %in% c(0, 1)) { #if hypothesis is numeric, which it is
      # last is complement
      if (true_hypothesis == 1) {
## !!!!!!!!!!!!!!! ##
        # All indices except last (complement) count as "true", since bain makes
        # two hypotheses out of one(!), might not be useful when generalizing!
        true_index <- seq_len(length(pmpc) - 1)
      } else {
        # Complement is last
        true_index <- length(pmpc)
      }
    } else {
      true_index <- as.integer(true_hypothesis)
      if (true_index < 1 || true_index > length(pmpc)) {
        stop("true_hypothesis index out of range")
      }
    }
  }
  else { #if hypothesis is neither character nor numeric raise error
    stop("true_hypothesis must be numeric (0/1 or index) or char. (name: H0)")
  }
  
################################################################################
# Rules for decision making
################################################################################
  
  if (cutoff == "regular") {
    # select most supported
    selected <- which.max(pmpc)
  } else if (cutoff == "unusual") {
    # select based on cutoff value
    combined_pmp <- sum(pmpc[true_index], na.rm=TRUE) #bain splits hypothesis
    if (combined_pmp >= threshold) {
      selected <- which.max(pmpc) #select after passing the threshold
    } else {
      selected <- NA
    }
  } else {
    stop("cutoff must be 'regular' or 'unusual'")
  }
  
################################################################################
# Decision making
################################################################################
    # No decision = incorrect
  if (is.na(selected)) return(1)
    # Correct if selected matches true_index
  if (selected %in% true_index) {
    return(0)  # correct decision
  } else {
    return(1)  # incorrect decision
  }
  
  
}

###############################################################################
bain_preference_old <- function(bain_obj, true_hypothesis){
  # catch some errors I encountered with bain objects
  if (is.null(bain_obj) || is.null(bain_obj$fit) || length(bain_obj$fit$PMPc) == 0) {
    message("Warning: bain_obj is invalid, returning NA")
    print(bain_obj)
    return(as.numeric(NA))
  }
  
  
  # Identify which hypothesis has the highest PMP
  selected <- which.max(bain_obj$fit$PMPc)
  hc_index <- length(bain_obj$fit$PMPc)  # Complement is last
  
  if (true_hypothesis == 0) {
    # Correct if complement is selected
    return(ifelse(selected == hc_index, 0, 1))
  }
  
  if (true_hypothesis == 1) {
    # Incorrect if complement is selected
    return(ifelse(selected == hc_index, 1, 0))
  }
  
  
}