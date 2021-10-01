
# look_ncols_excel() -----------------------------------------------------------

#' Look up the number of columns in an excel sheet
#'
#' \code{look_ncols_excel} is a function that reads in the first two lines of an
#' excel sheet and returns the number of columns. It is used as a helper
#' function to set the correct column types when importing all the time series
#' data downloaded from Lipper. The reason for this is that the first column in
#' excel output from Lipper contains the row numbers, but when the sheet has no
#' data, this first column will not be detected by \code{\link{read_excel}}.
#' Therefore the column types specified will vary depending on the output from
#' Lipper.
#'
#' Robust function: FALSE
#'
#' @param excel_file Complete file name (path) to the excel file
#'
#' @param sheet_num Integer value - the sheet number to read to R
#'
#' @return Returns the number of columns detected by \code{read_excel}
#'
#' @export

look_ncols_excel <- function(excel_file = NULL, sheet_num = NULL) {
  n_cols <-
    suppressMessages(readxl::read_excel(excel_file,
                                        sheet = sheet_num,
                                        n_max = 2)) %>%
    ncol

  return(n_cols)
}

# path_delist() ----------------------------------------------------------------

#' Delist paths
#'
#' \code{path_delist} is inspired by the functions in the \code{fs} package.
#' It takes a list of file paths and delist it and converts it to an fs object
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param path List of paths
#'
#' @return Vector of file paths of class \code{fs}
#'
#' @export

path_delist <- function(path) {

  path %>%
    purrr::flatten_chr() %>%
    unname() %>%
    fs::as_fs_path()
}

# read_colnames() --------------------------------------------------------------

#' Pull the column names from a data frame stored in fst format
#'
#' \code{read_colnames} takes a file path to a fst file, reads in the first
#' row of the data frame and returns the column names.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param filep Character vector of a single file path
#'
#' @return Vector with column names
#'
#' @export

read_colnames <- function(filep = NULL) {

  if (is.null(filep)) {
    warning("No file path found, returning NA", call. = F)
    return(NA_character_)
  }

  if (length(filep) > 1) {
    warning(paste("This function does not support multiple files",
                  "the first file is used", sep = " "), call. = F)

    filep <- filep[1]
  }

  if (!fs::is_file(filep)) {
    warning("File does not exist, returning NA", call. = F)
    return(NA_character_)
  }

  file_ext <- fs::path_ext(filep)

  if (file_ext != "fst") {
    warning("File format is not fst, returning NA", call. = F)
    return(NA_character_)
  }

  fst::read_fst(filep, from = 1, to = 1) %>%
    colnames()

}

# read_nrows() -----------------------------------------------------------------

#' Pull the number of rows from a data frame stored in fst format
#'
#' \code{read_nrows} takes a file path to a fst file, reads in the first
#' column of the data frame and returns the number of rows. It relies on
#' \code{\link{read_colnames}} to get the first column name of the data frame.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param filep Character vector of a single file path
#'
#' @return Numeric - number of rows
#'
#' @export

read_nrows <- function(filep = NULL) {

  if (is.null(filep)) {
    warning("No file path found, returning NA", call. = F)
    return(NA_character_)
  }

  if (length(filep) > 1) {
    warning(paste("This function does not support multiple files",
                  "the first file is used", sep = " "), call. = F)

    filep <- filep[1]
  }

  if (!fs::is_file(filep)) {
    warning("File does not exist, returning NA", call. = F)
    return(NA_character_)
  }

  file_ext <- fs::path_ext(filep)

  if (file_ext != "fst") {
    warning("File format is not fst, returning NA", call. = F)
    return(NA_character_)
  }

  filep %>%
    read_colnames() %>%
    .data[1] %>%
    fst::read_fst(filep, columns = .data) %>%
    nrow()
}


