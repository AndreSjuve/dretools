#' Find the Closest Date in a Date Column
#'
#' This function finds the closest date in a given date column for each date in a target vector. The closest date is determined by comparing the absolute differences between the target dates and the dates in the provided column.
#' It returns the closest date from the date column for each target date, taking into account whether the target date is before the first or after the last date in the column.
#'
#' @param target_dates A vector of target dates (class `Date`) for which the closest dates are to be found.
#' @param date_column A vector of dates (class `Date`) representing the available date column. The function assumes that this column is sorted in ascending order.
#'
#' @return A vector of `Date` values representing the closest dates in the `date_column` to each of the `target_dates`.
#' If the closest date lies before the first date or after the last date in `date_column`, it returns the first or last date accordingly.
#'
#' @details The function uses `findInterval()` to identify the closest date index in the `date_column`. It then compares the differences between the target dates and the closest dates before and after each target date, selecting the closest one.
#' It also handles edge cases where the target date is before the first date or after the last date in the column.
#'
#' @note The `date_column` must be sorted in ascending order for the function to work correctly. If the `date_column` is not sorted, the results may be incorrect. Additionally, the input dates are validated to ensure they are in the correct `Date` format.
#'
#' @warnings If any target dates fall outside the range of the `date_column`, the function will warn the user that some target dates are before the first or after the last date in the column.
#'
#' @examples
#' # Example with a target date and a sorted date column
#' target_dates <- as.Date(c('2023-05-01', '2023-06-15'))
#' date_column <- as.Date(c('2023-01-01', '2023-03-01', '2023-05-01', '2023-07-01'))
#' find_closest_date(target_dates, date_column)
#'
#' @export
find_closest_date <- function(target_dates, date_column) {
  # Ensure inputs are in Date format
  target_dates <- as.Date(target_dates)
  date_column <- sort(as.Date(date_column))

  # Check if the inputs are valid dates
  if (any(is.na(target_dates))) {
    cli_abort("Some target dates are not valid dates.")
  }
  if (any(is.na(date_column))) {
    cli_abort("Some dates in the date column are not valid dates.")
  }

  # Find the closest index using findInterval
  closest_index <- findInterval(target_dates, date_column)

  # Handle cases where target_date is outside the range of date_column
  if (any(closest_index == 0)) {
    cli_warn("Some target dates are before the first date in the date column.")
    # Target date is before the first date
    closest_index[closest_index == 0] <- 1
  }
  if (any(closest_index > length(date_column))) {
    cli_warn("Some target dates are after the last date in the date column.")
    # Target date is after the last date
    closest_index[closest_index > length(date_column)] <- length(date_column)
  }

  # Calculate differences for both the closest previous and next dates
  lower_diff <- abs(as.numeric(target_dates - date_column[closest_index]))
  upper_diff <- if_else(closest_index < length(date_column),
                        abs(as.numeric(target_dates - date_column[closest_index + 1])),
                        Inf)

  # Choose the closest date, comparing the lower and upper differences
  closest_dates <- if_else(upper_diff < lower_diff,
                           date_column[closest_index + 1],
                           date_column[closest_index])

  return(closest_dates)
}
