
# conditional_drop() -----------------------------------------------------------

#' Drop columns from data frame if they exist
#'
#' \code{conditional_drop} removes a set of columns passed by a character vector
#' if they exist in the data frame. If none exist, the original dataframe is
#' returned
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param data Data frame
#'
#' @param cols Character vector with names of one or more columns to drop
#'
#' @return \code{conditional_drop} returns the data frame without those columns
#' that do exist.
#'
#' @export

conditional_drop <- function(data, cols) {

  to_drop <-
    cols[which(cols %in% colnames(data))]

  if (length(to_drop) == 0) {
    message("None of the specified columns exist in dataset")
    return(data)
  } else {
    data <-
      data %>%
      dplyr::select(-to_drop)
  }
  return(data)
}

# filter_na_df() ---------------------------------------------------------------

#' Filter away NA rows for multiple columns at at time
#'
#' \code{filter_na_df} is a function that takes a vector of variable names
#' and filters away all NA values for those columns.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param data Data frame or tibble
#'
#' @param cols Character vector of one or more column names found in \code{data}
#'
#' @return Filtered data frame
#'
#' @importFrom dplyr filter_at

filter_na_df <- function(data, cols) {
  data <- dplyr::filter_at(data, cols, ~!is.na(.))
}


# has_duplicates() -------------------------------------------------------------

#' Are there duplicate values in your data?
#'
#' \code{has_duplicates} checks if there are duplicate rows in your dataframe,
#' unconditional on grouping.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param data Dataframe/tibble
#' @param ...  Other arguments
#'
#' @return TRUE if there are duplicate rows, otherwise FALSE
#'
#' @export

has_duplicates <- function(data, ...) {
  if (nrow(data) > nrow(dplyr::distinct(data))) return(TRUE)
  FALSE
}


# has_group_duplicates() -------------------------------------------------------

#' Check if your data frame contains duplicate rows by group
#'
#' \code{has_group_duplicates} checks if the data frame has duplicate values
#' when grouping by selection of variables given by \code{...}.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param data Dataframe/tibble
#' @param ...  Grouping variables
#'
#' @return TRUE if \code{data} has duplicates, otherwise FALSE
#'
#' @export

has_group_duplicates <- function(data, ...) {

  grouping_vars <- dplyr::vars(...)

  n_dups <-
    data %>%
    dplyr::select(!!! grouping_vars) %>%
    dplyr::group_by(!!! grouping_vars) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    nrow()

  if (n_dups != 0) return(TRUE)

  FALSE
}


# impute_missing_values() ------------------------------------------------------

#' Interpolate missing values using mean
#'
#' \code{impute_missing_values} is a function that takes a numeric vector with
#' missing data and replaces the NAs with the mean of the last value on the
#' left side and the first non-NA value on the right. It does this only for
#' repeated values of NA smaller than size \code{max_na}. This means that if
#' \code{max_na = 4}, the function will only replace missing values if there
#' are four or fewer missing values between the last non NA on the left and
#' the first non NA on the right.
#'
#' @param data Numeric vector with missing data
#' @param max_na Maximum number of NA in a row and still fill in values
#'
#' @return Numeric vector with NA values in groups <= max_na filled in
#' @export
#'
#' @importFrom purrr map_dbl map2_dbl map2
#'
#' @examples
#' \dontrun{
#' x <-
#' c(1, 1, NA, NA, NA, NA, 2, 2, 5, NA, NA, 6, 7, NA, 8, 9, NA,
#'   NA, NA, NA, NA, 10, 10, 13, NA, NA, 14, NA, NA, NA, NA, NA, NA, NA, 12)
#'
#' impute_missing_values(data = x)
#'
#' }

impute_missing_values <- function(data, max_na = 4) {

  n             <- length(data)
  first_obs_idx <- min(which(!is.na(data)))
  last_obs_idx_to_fill  <- max(which(!is.na(data)))

  if (first_obs_idx != 1) {
    data <- data[-c(1:(first_obs_idx - 1))]
  }

  last_obs_idx  <- max(which(!is.na(data)))

  if (last_obs_idx != n) {
    data <- data[-c((last_obs_idx + 1):n)]
  }

  na_pos       <- which(is.na(data))
  na_runs      <- rle(diff(na_pos))
  long_na_runs <- which(na_runs$lengths >= max_na)

  if (length(long_na_runs) > 0) {

    start_pos_long_na <-
      purrr::map_dbl(long_na_runs, ~{sum(na_runs$lengths[1:(.x - 1)]) + 1})

    # If missing values at the start, change first starting point long_na
    if (long_na_runs[1] == 1 & is.na(data[1]) ) {
      start_pos_long_na[1] <- 1
    } else if (long_na_runs[1] == 1 & !is.na(data[1])) {
      start_pos_long_na[1] <- 2
    }

    end_pos_long_na <-
      purrr::map2_dbl(.x = long_na_runs,
                      .y = start_pos_long_na, ~{na_runs$lengths[.x] + .y})

    na_rm_seq <-
      purrr::map2(.x = start_pos_long_na,
                  .y = end_pos_long_na, ~{.x:.y}) %>%
      purrr::reduce(c)

    na_pos <- na_pos[-na_rm_seq]
  }

  if (length(na_pos) %in% c(0, n)) {

    data <- c(rep(NA, first_obs_idx - 1),
              data,
              rep(NA, n - last_obs_idx_to_fill))


    return(data)
  }

  non_na_pos <- c(1:n)[which(1:n %nin% na_pos)]
  intervals  <- findInterval(na_pos, non_na_pos, all.inside = TRUE)
  tbl        <- table(intervals)
  intervals  <- intervals[intervals %in% names(tbl[tbl <= max_na])]
  left_pos   <- non_na_pos[pmax(1, intervals)]
  right_pos  <- non_na_pos[pmin(n, intervals + 1)]
  #left_dist  <- na_pos - left_pos
  #right_dist <- right_pos - na_pos

  if (length(left_pos) != 0 & length(right_pos) != 0) {

    data[na_pos] <- 0.5 * (data[left_pos] + data[right_pos])
  }


  data <- c(rep(NA, first_obs_idx - 1), data, rep(NA, n - last_obs_idx_to_fill))

  data

}

# impute_na() ------------------------------------------------------------------

#' Impute missing value based on mean
#'
#' \code{impute_na} is a function that takes a numeric vector \code{x} and it
#' inserts the average value of the values on the right and left of the missing
#' observations. If there is data on both sides a number is returned, if not NA.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param x Numeric vector
#'
#' @return Returns \code{x} with inserted missing values if one or more of
#'  them have observations on both sides
#'
#' @export

impute_na <- function(x) {

  na_pos    <- which(is.na(x))
  n_missing <- length(na_pos)
  n_obs     <- length(x)

  if (n_missing == 0 | n_missing == n_obs) return(x)

  if (na_pos[1] == 1) {

    imputed <- c(NA, 0.5 * (x[na_pos[-1] - 1] + x[na_pos[-1] + 1]))

  } else {
    imputed <- 0.5 * (x[na_pos - 1] + x[na_pos + 1])
  }

  x[na_pos] <- imputed

  x

}

# lag_cols() -------------------------------------------------------------------

#' Lag multiple columns at at time
#'
#' \code{lag_cols} is a function that takes a vector of variable names
#' and lags them all.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param data Data frame or tibble
#'
#' @param cols Character vector of one or more column names found in \code{data}
#'
#' @param n Variable in lag function, number of periods to lag
#'
#' @return Lagged data frame
#'
#' @importFrom dplyr lag

lag_cols <- function(data, cols, n = 1) {
  data <- dplyr::mutate_at(data, cols, ~dplyr::lag(., n = n))
}


# left_join_with_null() --------------------------------------------------------

#' Handle a data frame being NULL when merging
#'
#' \code{left_join_with_null} does not throw an error if \code{x} or \code{y} is
#' a NULL. In that case it returns the data frame that is not NULL. If both are
#' NULL, then NULL is returned. This function is based on the
#' \code{\link{left_join}} function from dplyr package.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x Data frame
#'
#' @param y Data frame
#'
#' @param by Character vector of common columns for \code{x} and \code{y}
#'
#' @return If both \code{x} and \code{y} is NULL, then NULL is returned. If
#'   either \code{x} or \code{y} is NULL, the non-NULL object is returned.
#'   Finally, if both \code{x} and \code{y} is not NULL, then the merged data
#'   frame is returned.
#'
#' @export

left_join_with_null <- function(x = NULL, y = NULL, by = NULL) {

  if (is.null(by)) {
    message("Beware that common variables (by) is not supplied")
  }

  if (is.null(x) & is.null(y)) {
    return(NULL)
  } else if (!is.null(x) & is.null(y)) {
    return(x)
  } else if (is.null(x) & !is.null(y)) {
    return(y)
  } else{
    if (is.null(by)) {
      dplyr::left_join(x,y)
    } else {
      dplyr::left_join(x,y, by = by)
    }
  }
}







