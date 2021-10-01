
# add_stars() ------------------------------------------------------------------

#' Add stars to statistical estimate
#'
#' \code{add_stars} is a function that adds stars to statistical estimates for
#' easy addition in latex tables.
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param estimates Numeric, vector with statistical estimates
#' @param p_vals    Numeric, vector with p-values for each element in
#'   \code{estimates}
#'
#' @importFrom rlang .data
#'
#' @return Character, vector with estimates and stars
#' @export


add_stars <- function(estimates, p_vals) {

  if (length(estimates) != length(p_vals)) {
    stop("estimates and p_vals of different lengths")
  }

  tibble::tibble(estimates = estimates,
                 p_vals = p_vals) %>%
    dplyr::mutate(est_stars = dplyr::case_when(
      p_vals <= 0.01 & !is.na(estimates) ~ paste0("$", estimates, "^{***}$"),
      p_vals <= 0.05 & !is.na(estimates) ~ paste0("$", estimates, "^{**}$"),
      p_vals <= 0.1  & !is.na(estimates) ~ paste0("$", estimates, "^{*}$"),
      p_vals >  0.1  & !is.na(estimates) ~ paste0("$", estimates, "$"),
      TRUE ~ NA_character_,
    )) %>%
    dplyr::select(.data$est_stars) %>%
    dplyr::pull()


}


# alligator_remover() ----------------------------------------------------------

#' Remove alligator signs
#'
#' \code{alligator_remover} vectorised function that removes "<" and ">" from
#' strings and replaces spaces between words with underscores (_).
#'
#' Used in scripts: {01_process_tables.R, 03_process_ts.R}
#' Robust function: TRUE
#'
#' @param col_names Character string (vector with strings)
#'
#' @return If input is single string a single string is returned. If input is
#'   vector of strings, a character vector is returned.
#' @export

alligator_remover <- function(col_names = NULL) {

  if (is.null(col_names)) {

    warning("no string passed, returning NA")

    return(NA_character_)

  }

  if (class(col_names) != "character") {

    col_names <-
      col_names %>%
      as.character()

    message("col_names class converted to character")

  }

  col_names %>%
    gsub(pattern = "<", replacement = "", x = .data) %>%
    gsub(pattern = ">", replacement = "", x = .data) %>%
    gsub(pattern = " ", replacement = "_")

}

# camel_to_snake() -------------------------------------------------------------

#' Convert camel case words to snake case words
#'
#' \code{camel_to_snake} is a function that splits a word by capital letters,
#' converts all letters to lower case and bind the splitted words together by
#' an underscore, e.g., SecurityId becomes security_id. It is primarily used
#' to fix the column names in the raw data downloaded from Børsprosjektet.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param x Character vector of one or more elements.
#'
#' @return Character vector of camel case words.
#' @export
#'
#' @examples
#' \dontrun{
#' camel_to_snake("SecurityID")
#'
#' camel_to_snake("SecId", "FooBar")
#' }

camel_to_snake <- function(x) {
  x %>%
    strsplit("(?<=[a-z])(?=[A-Z])", perl = TRUE) %>%
    purrr::map(~paste0(tolower(.x), collapse = "_")) %>%
    unlist()
}

# standardise_character() ------------------------------------------------------

#' Standardise a character string
#'
#' \code{standardise_character} takes a character vector and transform all
#' elements to lowercase, removes trailing, leading and superflous whitespaces
#' and replaces all single spaces with an underscore.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param x Character vector of one or more strings
#'
#' @return Character vector
#'
#' @export

standardise_character <- function(x) {
  stringr::str_replace_all(stringr::str_squish(tolower(x)), " ", "_")
}


# str_remove_after() -----------------------------------------------------------

#' Remove everything after the first separator in a string
#'
#' \code{str_remove_after} is a (vectorised) function that removes everything
#' that appears after a chosen separator in a text string. This is primarily
#' used to remove characters in security names that appear after the separator
#' in order to make it easier to match companies by name.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param string Input vector.
#'
#' @param sep Where to split the string.
#'
#' @return Character vector with only the parts appearing before the separator.
#'
#' @export

str_remove_after <- function(string, sep) {
  stringr::str_squish(purrr::map_chr(stringr::str_split(string, sep), ~.x[1]))
}


# str_remove_decimals() --------------------------------------------------------

#' Remove decimals from large numbers in string
#'
#' \code{str_remove_decimals} is a function that for a character vector where
#' each element is a single word or number identifies all decimal numbers
#' (detected by a period .) and if the preceeding number of digits is larger
#' than one, i.e., the number in the text is >=10, then the decimals are removed
#' from the number. The intended use is to clean output to latex tables such
#' that decimals for large numbers are removed. It is important that the numbers
#' are stored in a single element in the string vector.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param string Character string
#'
#' @param pattern Number pattern to look for
#'
#' @param n_digits Indicating how large the number must be before removing
#'   decimals. n_digits = 2 will remove decimals for numbers greater than or
#'   equal to 10, n_digits = 3 for numbers greater than or equal to 100 etc.
#'
#' @return Character string with decimals removed for large numbers
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' str_remove_decimals("Hello, world 1,000.000")
#' str_remove_decimals("1.000")
#' str_remove_decimals("10.000")
#' str_remove_decimals("1,000.00")
#' str_remove_decimals(c("Hello, world", "1,000.00"))
#' }

str_remove_decimals <- function(string, pattern = NULL, n_digits = 2) {

  n_digits <- as.character(n_digits)

  if (is.null(pattern)) {

    pattern <-
      "($?-$?)?([0-9]{1,},)*\\d+(\\.\\d+)?"

    #pattern <-
    #  paste0("($?-$?)?([0-9]{1,},)*[0-9]{", n_digits, ",}(\\.\\d+)?")

    #pattern <-
    #  paste0("($?-$?)?([0-9]{1,},)*[0-9]{", n_digits, ",}(?=\\.)")
  }

  purrr::map_chr(string, ~{

    if (stringr::str_detect(.x, pattern, negate = T)) {
      return(.x)
    } else {

      non_numeric_start <-
        stringr::str_extract(.x, "^\\D+")

      if (!is.na(non_numeric_start) & non_numeric_start == "-") {
        non_numeric_start <- ""
      }

      non_numeric_end <-
        stringr::str_extract(.x, "\\D+$")

      number <-
        as.numeric(stringr::str_remove_all(stringr::str_extract(.x, pattern), ","))

      if (nchar(trunc(abs(number))) >= n_digits) {
        number <- format(round(number), big.mark = ",")
      } else {
        number <- number
      }

      paste0(
        coalesce(non_numeric_start, ""),
        number,
        coalesce(non_numeric_end, "")
      )


    }


  })

}


#str_remove_decimals <- function(string, pattern = NULL, n_digits = 2) {
#
#  n_digits <- as.character(n_digits)
#
#  if (is.null(pattern)) {
#    pattern <-
#      paste0("($?-$?)?([0-9]{1,},)*[0-9]{", n_digits, ",}(?=\\.)")
#  }
#
#  map_chr(string, ~{
#    if (str_detect(.x, pattern, negate = T)) {
#      return(.x)
#    }
#
#    if (str_detect(.x, "[^(\\d|\\.)]")) {
#
#      paste0(
#        str_extract(.x, "^\\D+"),
#        unlist(str_extract(.x, pattern)),
#        str_extract(.x, "\\D+$"))
#
#    } else {
#      unlist(str_extract(.x, pattern))
#    }
#
#  })
#
#}


# str_remove_decimals_all() ----------------------------------------------------

#' Remove decimals from large numbers in multiple long strings
#'
#' \code{str_remove_decimals_all} is a function that for a character vector
#' where each element is a string with multiple words or numbers identifies all
#' decimal numbers (detected by a period .) and if the preceeding number of
#' digits is larger than one, i.e., the number in the text is >=10, then the
#' decimals are removed from the number. The intended use is to clean output to
#' latex tables such that decimals for large numbers are removed. It is
#' important that the numbers are stored in a single element in the string
#' vector. The input here is typically the output from \code{stargazer}.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param string Character vector
#'
#' @param pattern Number pattern to look for
#'
#' @param n_digits Indicating how large the number must be before removing
#'   decimals. n_digits = 2 will remove decimals for numbers greater than or
#'   equal to 10, n_digits = 3 for numbers greater than or equal to 100 etc.
#'
#' @param split_pattern Pattern to split each element of string vector by. This
#'   argument should ensure that each number stored in each string element is
#'   isolated by itself. In latex output, this is achieved by **&**
#'
#' @return Character vector with strings where decimals are removed for large
#'   numbers
#'
#' @export
#'
#' @examples
#' \dontrun{
#' str_remove_decimals_all(c("Hello & 1,000.000", "My text"))
#' str_remove_decimals_all("Hello; 10.000", "1.000 is low")
#' }


str_remove_decimals_all <-
  function(string, pattern = NULL, split_pattern = NULL, n_digits) {

    if (is.null(split_pattern)) {
      split_pattern <- "&"
    }

    purrr::map_chr(string, ~{

      if (stringr::str_detect(.x, split_pattern)) {

        out <-
          stringr::str_split(.x , split_pattern) %>%
          unlist() %>%
          stringr::str_squish() %>%
          str_remove_decimals(., pattern = pattern,
                                       n_digits = n_digits) %>%
          paste(., collapse = " & ")

        if (stringr::str_detect(out, "\\\\s*$", negate = T)) {
          out <- paste(out, "\\\\", collapse = " ")
        }

      } else {

        out <- .x

        #out <- str_remove_decimals(.x, pattern = pattern, n_digits = n_digits)

      }

      out

    })

  }

































