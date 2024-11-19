#' Calculate Confidence Intervals for the Mean
#'
#' This function computes confidence intervals for the mean of a sample, optionally using the population standard deviation.
#'
#' @param confidence Confidence level (default 0.95).
#' @param sample_data Numeric vector of sample data.
#' @param pop_sd Population standard deviation (default NULL).
#' @param norm_dist Logical, whether to assume normal distribution (default FALSE).
#' @return A data frame with sample mean, confidence bounds and margin of error.
#' @export
get_ci_mean <- function(confidence = 0.95, sample_data, pop_sd = NULL, norm_dist = FALSE) {
  ...
}
get_ci_mean <- function(confidence = 0.95, sample_data, pop_sd = NULL, pop_mean = NULL, norm_dist = FALSE) {
  # Validate inputs
  if (length(sample_data) == 0) stop("Sample data must not be empty.")
  if (confidence <= 0 || confidence >= 1) stop("Confidence level must be between 0 and 1.")

  # Sample statistics
  n <- length(sample_data)
  sample_mean <- mean(sample_data)
  sample_sd <- sd(sample_data)

  # Determine the appropriate standard deviation
  sd_to_use <- if (!is.null(pop_sd)) pop_sd else sample_sd

  # Determine the critical value
  alpha <- 1 - confidence
  if (!norm_dist && is.null(pop_sd)) {
    # Use t-distribution if the population standard deviation is unknown
    critical_value <- qt(1 - alpha / 2, df = n - 1)
  } else {
    # Use z-distribution otherwise
    critical_value <- qnorm(1 - alpha / 2)
  }

  # Margin of error
  margin_of_error <- critical_value * (sd_to_use / sqrt(n))

  # Confidence interval
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error

  return(data.frame(
    Mean = sample_mean,
    Lower_Bound = lower_bound,
    Upper_Bound = upper_bound,
    Margin_of_Error = margin_of_error
  ))
}
