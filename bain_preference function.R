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
  
  # get all PMPb´s of all hypotheses
  pmpb <- as.numeric(bain_obj$fit$PMPb)
  
################################################################################
# Give names to hypotheses
################################################################################
  if (is.null(names(bain_obj$fit$PMPb))) {
    names(pmpb) <- paste0("H", seq_along(pmpb))
    names(pmpb)[length(pmpb)] <- "Hc"  # last one is complement, second to last is unconstraint
  } else {
    names(pmpb) <- names(bain_obj$fit$PMPb)
  }
  
  # find indices of Hu and Hc explicitly
  hu_index <- which(names(pmpb) == "Hu")
  hc_index <- which(names(pmpb) == "Hc")

################################################################################
# Matching the hypotheses names to their index
################################################################################

  if (is.character(true_hypothesis)) { #if hypothesis is a character
    # give true index index of where true_hypothesis matches the name of a PMPc
    true_index <- match(true_hypothesis, names(pmpb))
    if (is.na(true_index)) {
      stop("true_hypothesis (character) not found in PMP names")
    }
  }
  else if (is.numeric(true_hypothesis)) {
    if (true_hypothesis == 0) {
      # H0 true → unconstrained (Hu)
      true_index <- hu_index
    } else if (true_hypothesis == 1) {
      # H1 true → all constrained hypotheses (exclude Hu and Hc)
      true_index <- setdiff(seq_along(pmpb), c(hu_index, hc_index))
    } else {
      stop("true_hypothesis must be 0 (Hu) or 1 (constraints)")
    }
  } 
  else {
    stop("true_hypothesis must be numeric (0/1) or character (name)")
  }
  
################################################################################
# Rules for decision making
################################################################################
  
  if (cutoff == "regular") {
    # select most supported
    selected <- which.max(pmpb)
  } else if (cutoff == "unusual") {
    # select based on cutoff value
    combined_pmp <- sum(pmpb[true_index], na.rm=TRUE) #bain splits hypothesis
    if (combined_pmp >= threshold) {
      selected <- which.max(pmpb) #select after passing the threshold
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
  if (is.na(selected)) return(0)
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