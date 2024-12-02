library(testthat)
library(cli)
library(dretools)

# Test for find_closest_date function
test_that("find_closest_date works correctly with valid inputs", {
  # Define example dates
  target_dates <- as.Date(c("2023-05-01", "2023-06-15"))
  date_column <- as.Date(c("2023-01-01", "2023-03-01", "2023-05-01", "2023-07-01"))

  # Expected closest dates
  expected_output <- as.Date(c("2023-05-01", "2023-07-01"))

  # Test if the function returns the correct closest dates
  result <- find_closest_date(target_dates, date_column)
  expect_equal(result, expected_output)
})

test_that("find_closest_date returns the correct dates when target is out of range", {
  # Target dates before the first date in the column and after the last date
  target_dates <- as.Date(c("2022-12-01", "2023-08-01"))
  date_column <- as.Date(c("2023-01-01", "2023-03-01", "2023-05-01", "2023-07-01"))

  # Expected closest dates
  expected_output <- as.Date(c("2023-01-01", "2023-07-01"))

  # Test if the function returns the correct closest dates
  result <- find_closest_date(target_dates, date_column)
  expect_equal(result, expected_output)
})

test_that("find_closest_date handles NA values in target_dates and date_column", {
  # Include NA values in target_dates
  target_dates <- as.Date(c("2023-05-01", NA))
  date_column <- as.Date(c("2023-01-01", "2023-03-01", "2023-05-01", "2023-07-01"))

  # Check if the function throws an error for NA values in target_dates
  expect_error(find_closest_date(target_dates, date_column),
               "Some target dates are not valid dates.")

  # Include NA values in date_column
  target_dates <- as.Date(c("2023-05-01"))
  date_column <- as.Date(c("2023-01-01", "2023-03-01", NA, "2023-07-01"))

  # Check if the function throws an error for NA values in date_column
  expect_error(find_closest_date(target_dates, date_column),
               "Some dates in the date column are not valid dates.")
})

test_that("find_closest_date handles target dates before and after the date range correctly", {
  target_dates <- as.Date(c("2022-12-01", "2023-06-01"))
  date_column <- as.Date(c("2023-01-01", "2023-04-01", "2023-08-01"))

  # Expected closest dates
  expected_output <- as.Date(c("2023-01-01", "2023-04-01"))

  # Test if the function returns the correct closest dates
  result <- find_closest_date(target_dates, date_column)
  expect_equal(result, expected_output)
})

test_that("find_closest_date correctly warns about out-of-range dates", {
  target_dates <- as.Date(c("2022-12-01", "2023-08-01"))
  date_column <- as.Date(c("2023-01-01", "2023-03-01", "2023-05-01", "2023-07-01"))

  # Capture the warnings
  expect_warning(find_closest_date(target_dates, date_column),
                 "Some target dates are before the first date in the date column.")

  # Test for target dates after the last date
  target_dates <- as.Date(c("2023-08-01"))
  expect_warning(find_closest_date(target_dates, date_column),
                 "Some target dates are after the last date in the date column.")
})

test_that("find_closest_date handles empty date columns gracefully", {
  target_dates <- as.Date(c("2023-05-01"))
  date_column <- as.Date(c())

  # Test if the function handles an empty date column
  expect_error(find_closest_date(target_dates, date_column),
               "Some dates in the date column are not valid dates.")
})
