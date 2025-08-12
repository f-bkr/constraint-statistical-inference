# This is the function for the bain function

bain_function <- function(est_AdjMeans, hypothesis, n, VCOV_AdjMeans, true_hypothesis) {
  tryCatch(
    {
      
      message("Running bain with:")
      message("est_AdjMeans: ", paste(round(est_AdjMeans, 4), collapse = ", "))
      message("hypothesis: ", hypothesis)
      message("n: ", n)
      message("VCOV_AdjMeans: ", paste(round(as.vector(VCOV_AdjMeans), 4), collapse = ", "))
      
      bain_AdjMeans <- bain(
        x = est_AdjMeans,
        hypothesis = hypothesis,
        n = n,
        Sigma = list(VCOV_AdjMeans),
        group_parameters = 3,
        joint_parameters = 0
      )
      
      return(bain_AdjMeans)
    },
    error = function(e) {
      message("Bain error: ", e$message)
      message("---- Debug info ----")
      message("est_AdjMeans: ", paste(round(est_AdjMeans, 4), collapse = ", "))
      message("hypothesis: ", hypothesis)
      message("n: ", n)
      message("VCOV_AdjMeans: ", paste(round(as.vector(VCOV_AdjMeans), 4), collapse = ", "))
      
      return(NA)  # Still return NULL so bain_preference() sees invalid case
    }
  )
}

