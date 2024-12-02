#' Unit tests for find_closest_date function
library(testthat)
library(cli)

# Test function: find_closest_date
test_that("find_closest_date works correctly with valid input", {
  target_dates <- as.Date(c('2020-01-01', '2022-06-15', '2023-11-01'))
  date_column <- as.Date(c('2019-01-01', '2020-06-01', '2021-01-01', '2022-01-01'))

  result <- find_closest_date(target_dates, date_column)

  # Expected closest dates (based on the input data)
  expected_result <- as.Date(c('2020-06-01', '2022-01-01', '2022-01-01'))

  expect_equal(result, expected_result)
})

test_that("find_closest_date handles target dates before the first date", {
  target_dates <- as.Date(c('2018-12-01', '2019-05-01'))
  date_column <- as.Date(c('2019-01-01', '2020-06-01', '2021-01-01', '2022-01-01'))

  expect_warning(
    result <- find_closest_date(target_dates, date_column),
    "Some target dates are before the first date in the date column."
  )

  expected_result <- as.Date(c('2019-01-01', '2019-01-01'))
  expect_equal(result, expected_result)
})

test_that("find_closest_date handles target dates after the last date", {
  target_dates <- as.Date(c('2023-05-01', '2024-07-01'))
  date_column <- as.Date(c('2019-01-01', '2020-06-01', '2021-01-01', '2022-01-01'))

  expect_warning(
    result <- find_closest_date(target_dates, date_column),
    "Some target dates are after the last date in the date column."
  )

  expected_result <- as.Date(c('2022-01-01', '2022-01-01'))
  expect_equal(result, expected_result)
})

test_that("find_closest_date throws error for invalid target dates", {
  target_dates <- c('2020-01-01', 'invalid-date', '2022-06-15')
  date_column <- as.Date(c('2019-01-01', '2020-06-01', '2021-01-01', '2022-01-01'))

  expect_error(find_closest_date(target_dates, date_column),
               "Some target dates are not valid dates.")
})

test_that("find_closest_date throws error for invalid date column", {
  target_dates <- as.Date(c('2020-01-01', '2022-06-15'))
  date_column <- c('invalid-date', '2020-06-01', '2021-01-01', '2022-01-01')

  expect_error(find_closest_date(target_dates, date_column),
               "Some dates in the date column are not valid dates.")
})

test_that("find_closest_date handles edge case with a single date in the date column", {
  target_dates <- as.Date(c('2020-01-01', '2021-01-01', '2022-06-01'))
  date_column <- as.Date('2020-01-01')

  result <- find_closest_date(target_dates, date_column)
  expected_result <- as.Date(c('2020-01-01', '2020-01-01', '2020-01-01'))

  expect_equal(result, expected_result)
})

test_that("find_closest_date gives correct result for target date exactly equal to the last date in the column", {
  target_dates <- as.Date(c('2020-06-01', '2022-01-01'))
  date_column <- as.Date(c('2019-01-01', '2020-06-01', '2021-01-01', '2022-01-01'))

  result <- find_closest_date(target_dates, date_column)
  expected_result <- as.Date(c('2020-06-01', '2022-01-01'))

  expect_equal(result, expected_result)
})
