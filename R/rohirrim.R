#' Calculate Over-Prediction Rate (OPR), Under-Prediction Rate (UPR),
#' Potential Presence Increment (PPI), and Potential Absence Increment (PAI).
#'
#' This function takes an input object with components 'tp', 'fp', 'fn', and 'tn'
#' representing the counts of true positives, false positives, false negatives,
#' and true negatives, respectively. It calculates OPR, UPR, PPI, and PAI based
#' on the input counts and returns the results in a data frame.
#'
#' @param obj An object containing counts of true positives (tp), false positives (fp),
#'   false negatives (fn), and true negatives (tn).
#'
#' @return A data frame with columns for OPR, UPR, PPI, and PAI.
#'
#' @examples
#' obj <- list(tp = 10, fp = 5, fn = 3, tn = 20)
#' result <- rohirrim(obj)
#' print(result)
#'
#' @seealso
#' \code{\link{print}} for printing the result data frame.
#'
#' @keywords rohirrim metrics performance
#' @export
rohirrim <- function(obj) {
  # Input validation
  if (!all(c("tp", "fp", "fn", "tn") %in% names(obj))) {
    stop("Input object must contain 'tp', 'fp', 'fn', and 'tn' components.")
  }

  # Extract values
  tp <- obj$tp
  fp <- obj$fp
  fn <- obj$fn
  tn <- obj$tn

  # Avoid division by zero
  if (tp + fp == 0) {
    stop("The sum of 'tp' and 'fp' is zero. Cannot calculate OPR and PPI.")
  }
  if (fn + tn == 0) {
    stop("The sum of 'fn' and 'tn' is zero. Cannot calculate UPR and PAI.")
  }

  # Calculate metrics
  OPR <- fp / (tp + fp)
  UPR <- fn / (fn + tn)
  PPI <- (tp + fp) / (tp + fn) - 1
  PAI <- (fn + tn) / (fp + tn) - 1

  # Create data frame
  result_df <- data.frame(OPR = OPR,
                          UPR = UPR,
                          PPI = PPI,
                          PAI = PAI)

  return(result_df)
}

