
# nin() ------------------------------------------------------------------------

#' Opposite of in function
#'
#' @param x Elements to check that are not in \code{table}
#'
#' @param table Table to look up values \code{x} in
#'
#' @return Returns the elements of \code{x} not in \code{table}
#'
#' @export
#'
#' @examples
#' c(1, 2, 3) %nin% c(2, 3)

`%nin%` <- function(x, table){
  match(x, table, nomatch = 0L) == 0L
}
