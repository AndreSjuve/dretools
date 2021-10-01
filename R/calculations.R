
# ann_monthly_ret() ------------------------------------------------------------

#' Annualise monthly returns
#'
#' \code{ann_monthly_ret} annualises monthly returns, when the numbers are
#' expressed in percentages, i.e. between zero and 100.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x vector of percentage returns
#'
#' @return vector of annualised returns in percentage
#' @export

ann_monthly_ret <- function(x) {((1 + x / 100)^12 - 1) * 100}

# ann_sd() ---------------------------------------------------------------------

#' Annulise standard deviation
#'
#' \code{ann_sd} annualises the standard deviation based on scale
#' (monthly = 12, quarterly = 4, biannually = 2, daily = 252)
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x numeric vector
#'
#' @param scale time frequency of \code{x}
#'
#' @return vector of annualised standard deviations
#'
#' @export
#'
#' @importFrom stats sd

ann_sd <- function(x, scale = 12) {stats::sd(x) * sqrt(scale)}


# calc_log_return() ------------------------------------------------------------

#' Compute logarithmic net returns for a vector
#'
#' \code{calc_log_return} helper function for \code{compute_return}.
#' Takes every element of \code{x} except the first and divides by every
#' element of \code{x} excep the last and takes the log.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x numeric vector
#'
#' @return Vector of same length as \code{x}
#' @export

calc_log_return <- function(x) {
  x <- c(NA_real_, log(x[-1] / x[-length(x)]))
}


# calc_simple_return() ---------------------------------------------------------

#' Compute simple net returns for a vector
#' Used in scripts: {}
#'
#' \code{calc_simple_return} helper function for \code{\link{compute_return}}.
#' Takes every element of \code{x} except the first and divides by every
#' element of \code{x} excep the last and subtracts one (1).
#'
#' @param x numeric vector
#'
#' @return Vector of same length as \code{x}
#' @export

calc_simple_return <- function(x) {
  x <- c(NA_real_, (x[-1] / x[-length(x)]) - 1)
}


# compute_return() -------------------------------------------------------------

#' Compute simple or logarithmic returns
#'
#' \code{compute_return} computes simple or logarithmic returns for a vector
#' of values. The function uses \code{\link{calc_simple_return}} and
#' \code{\link{calc_log_return}}.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x numeric vector
#'
#' @param type either "simple" or "log"
#'
#' @return If \code{x} is a vector of length > 1, returns a vector of equal
#' length to input vector with returns and code{NA_real_} in first position
#' @export
#'
#' @importFrom stringr str_squish


compute_return <- function(x = NULL, type = c("simple", "log")) {

  if (is.null(x)) {
    warning("x is NULL, returning NULL", call. = F)
    return(NA_real_)
  }

  if (!is.numeric(x)) {
    stop("x is not numeric", call. = F)
  }

  if (length(x) <= 1) {

    warning("x is of length zero or one, returning NA")
    return(NA_real_)

  }

  type <-
    tolower(stringr::str_squish(type))

  if (!type %in% c("simple", "log")) {

    warning("type not simple or log, calculating simple returns", call. = F)
    x <- calc_simple_return(x)
  }

  if (type == "simple") {
    x <- calc_simple_return(x)
  } else if (type == "log") {
    x <- calc_log_return(x)
  }
}

# cumret() ---------------------------------------------------------------------

#' Compute the cumulative product of a vector
#'
#' \code{cumret} is a function that computes the cumulative product of a numeric
#' vector \code{x} and returns the last element.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x Numeric vector
#'
#' @return \code{cumret} returns the last value in the cumulative product vector
#'
#' @export

cumret <- function(x) {
  utils::tail((cumprod(x) - 1), 1)
}

# divide_cols_by() -------------------------------------------------------------

#' Divide a set of columns by a number
#'
#' \code{divide_cols_by} takes a \code{data frame} or \code{tibble} object,
#' a set of column names in a vector, any real number and divides the values
#' in the cols argument by that number and returns the tbl.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' TO IMPROVE:
#' * Check that data is not NULL, if return NULL
#' * Make column names not case sensitive
#' * Check which column(s) are and are not in the dataset, throw warning if
#'   any are missing, and operate only on those that are in the dataset
#' * Check the classes of the passed columns to ensure they are numeric and
#'   possible to divide by \code{by}. If not attempt to convert to numeric, else
#'   throw a warning that those columns are not operated on
#'
#' @param data Data frame or tibble object
#'
#' @param cols Vector of one or more columns in \code{data} to operate on
#'
#' @param by Number to divide by. Default is 100
#'
#' @return Data frame
#'
#' @export
#'
#' @importFrom dplyr mutate_at

divide_cols_by <- function(data = NULL, cols = NULL, by = 100) {
  data <-  mutate_at(data, cols, ~ . / by)
}


# length_na() ------------------------------------------------------------------

#' Calculate length of NAs in a vector
#'
#' \code{length_na} is a function that computes how many NA values there are
#' in a vector
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x Vector
#'
#' @return \code{length_na} returns the number of NAs in a vector.
#'
#' @export

length_na <- function(x){
  length(which(is.na(x)))
}







