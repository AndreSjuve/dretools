
# gen_missing_dates() ----------------------------------------------------------

#' Extract missing dates in a monthly sequence
#'
#' \code{gen_missing_dates} takes a sequence of dates and finds missing
#' month-end dates. It uses the function \code{\link{gen_month_ends}} to
#' generate a sequence of month-end dates between the first and last date in
#' \code{x}.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param x Vector of dates
#'
#' @return If multiple dates are missing, it returns a vector of dates. If one
#'   date is missing, it returns a single date and if no dates are missing it
#'   returns NA
#'
#' @export
#'
#' @importFrom lubridate ymd

gen_missing_dates <- function(x = NULL) {

  if (is.null(x)) {
    warning("x is NULL, returning NULL", call. = FALSE)
    return(NULL)
  }

  if (class(x)[1] != "Date") {
    x <- lubridate::date(x)
  }

  if (min(x, n.rm = T) < lubridate::date("1970-01-01")) {
    stop("Start date is before origin date, 01.01.1970", call. = FALSE)
  }

  missing_dates <-
    gen_month_ends(sdate = min(x),
                   edate = max(x))

  missing_dates <-
    missing_dates[which(missing_dates %nin% x)]

  if (length(as.character(missing_dates)) == 0) {

    message("No missing dates, returning NA")
    return(NA_real_)
  }
  missing_dates
}


# gen_month_ends() -------------------------------------------------------------

#' Sequence of monthly end dates
#'
#' \code{gen_month_ends} takes a start and end date and creates a sequence of
#' monthly dates, with the last day of each month.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param sdate Start date on "Y-m-d" format
#'
#' @param edate End date on "Y-m-d" format
#'
#' @return Sequence of dates
#'
#' @export
#'
#' @importFrom lubridate floor_date ceiling_date add_with_rollback
#' @importFrom zoo as.yearmon

gen_month_ends <- function(sdate = NULL, edate = NULL) {

  sdate <-
    lubridate::floor_date(lubridate::date(sdate), "month")

  edate <-
    lubridate::ceiling_date(lubridate::date(edate), "month")

  diff_months <-
    12 * as.numeric((zoo::as.yearmon(edate) - zoo::as.yearmon(sdate)))

  seq(lubridate::add_with_rollback(sdate, months(1)),
      length = diff_months,
      by = "1 month") - 1

}


# last_day() -------------------------------------------------------------------

#' Find last day of month
#'
#' \code{last_day} takes a single date or vector of dates and returns the last
#' date in the month.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param date Single date object or vector of dates
#'
#' @return If \code{date} is a single date, a single date is returned. If
#'   \code{date} is a vector of date objects a vector is returned.
#'
#' @export
#'
#' @importFrom lubridate days


last_day <- function(date) {
  lubridate::ceiling_date(date, "month") - lubridate::days(1)
}


# push_to_month_end() ----------------------------------------------------------

#' Convert date to the last day of the month
#'
#' \code{push_to_month_end} is a function that returns the date of the last
#' day of the month for a given date.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param date Date on format from Lubridate package
#'
#' @return Returns the date of the last day of the month
#'
#' @export

push_to_month_end <- function(date) {
  lubridate::ceiling_date(date, "month") - lubridate::days(1)
}

# unlist_dates() ---------------------------------------------------------------

#' Convert dates in list (data frame or list) to vector of dates
#'
#' \code{unlist_dates} converts dates stored in data frames or lists into
#' a vector while retaining the date format. It also unnames the dates and
#' uses the \code{lubridate} package to convert the dates to \code{ymd} format
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param x Data frame or list of dates
#'
#' @return Vector of dates on "%Y-%m-%d" format
#'
#' @export

unlist_dates <- function(x) {

  if (is.null(x)) {
    warning("x is NULL, returning NULL", call. = FALSE)
    return(NULL)
  }

  if (any(class(x) %in% c("tbl_df", "tbl", "data.frame"))) {
    if (ncol(x) > 1) {
      warning("x has more than one column, first one is used", call. = FALSE)
      x <- x[, 1]
    }
  }

  x %>%
    do.call("c", .data) %>%
    unname() %>%
    lubridate::ymd(.data, tz = "UTC")
}






