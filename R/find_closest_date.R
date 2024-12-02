#' Find the Closest Date
#'
#' This function finds the closest date from a target date to a date column. It checks if any target dates fall outside the range of the date column (either before the first date or after the last date) and provides warnings for such cases, printing the target dates in different colors based on their relation to the range of the date column.
#'
#' @param target_dates A vector of target dates to find the closest dates for. The dates should be in a format that can be coerced into `Date` type.
#' @param date_column A vector of dates representing the date column to compare against. The dates should be in a format that can be coerced into `Date` type.
#'
#' @return A vector of dates from the date column that are closest to each of the target dates.
#'
#' @details
#' - The function will warn and print all target dates that are before the first date in the date column in orange, with a message in green and bold/underlined.
#' - It will warn and print all target dates that are after the last date in the date column in navy blue, with a message in dark red, bold, and underlined.
#' - The closest date is selected based on the minimum difference between the target date and the available dates.
#' - If a target date falls outside the range of the date column, the closest date is assigned as either the first or last date in the column accordingly.
#' - The function uses `cli` package functionality for colorful text and styled warnings.
#'
#' @examples
#' \dontrun{
#' target_dates <- as.Date(c('2020-01-01', '2023-05-01', '2024-07-01'))
#' date_column <- as.Date(c('2019-01-01', '2020-06-01', '2021-01-01', '2022-01-01'))
#' find_closest_date(target_dates, date_column)
#' }
#'
#' @importFrom cli cli_abort cli_warn cli_text col_blue col_green col_yellow
#' @importFrom cli style_bold style_underline
find_closest_date <- function(target_dates, date_column) {

  # Ensure inputs are in Date format
  # Ensure inputs are in Date format using lubridate's parse_date_time
  target_dates <-
    suppressWarnings(
    lubridate::parse_date_time(target_dates,
                               orders = c("ymd", "mdy", "dmy")))
  date_column <-
    suppressWarnings(
    lubridate::parse_date_time(date_column,
                               orders = c("ymd", "mdy", "dmy")))

  # Convert to Date class to remove time information
  target_dates <- as.Date(target_dates)
  date_column <- as.Date(date_column)

  # Check if the inputs are valid dates
  if (any(is.na(target_dates))) {
    cli::cli_abort("Some target dates are not valid dates.")
  }
  if (any(is.na(date_column))) {
    cli::cli_abort("Some dates in the date column are not valid dates.")
  }


  # Find the closest index using findInterval
  closest_index <- findInterval(target_dates, date_column)

  # Handle cases where target_date is outside the range of date_column
  if (any(closest_index == 0)) {
    cli::cli_warn(
      cli::col_green(
        cli::style_bold(
          cli::style_underline(
            "Some target dates are before the first date in the date column."
          )
        )
      )
    )
    # Print out the target dates that are before the first date in orange
    before_first_date <- target_dates[closest_index == 0]
    cli::cli_text(cli::col_blue("Target dates before the first date:"))
    cli::cli_text(cli::col_green(paste(format(before_first_date, "%Y-%m-%d"), collapse = ", ")))
    closest_index[closest_index == 0] <- 1  # Target date is before the first date
  }

  # Identify target dates that are after the last date in the date column
  after_last_date <- target_dates > max(date_column)
  if (any(after_last_date)) {
    # Text in dark red and bold + underlined for warning
    cli::cli_warn(
      cli::col_yellow(
        cli::style_bold(
          cli::style_underline(
            "Some target dates are after the last date in the date column."
          )
        )
      )
    )
    # Print out the target dates that are after the last date in navy blue
    cli::cli_text(cli::col_blue("Target dates after the last date:"))
    cli::cli_text(cli::col_yellow(paste(format(target_dates[after_last_date], "%Y-%m-%d"), collapse = ", ")))
  }

  # Calculate differences for both the closest previous and next dates
  lower_diff <- abs(as.numeric(target_dates - date_column[closest_index]))
  upper_diff <- dplyr::if_else(closest_index < length(date_column),
                       abs(as.numeric(target_dates - date_column[closest_index + 1])),
                       Inf)

  # Choose the closest date, comparing the lower and upper differences
  closest_dates <- dplyr::if_else(upper_diff < lower_diff,
                           date_column[closest_index + 1],
                           date_column[closest_index])

  return(closest_dates)
}
