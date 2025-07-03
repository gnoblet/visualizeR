# not in
#' Not In Operator
#'
#' A negation of the `%in%` operator that tests if elements of `a` are not in `b`.
#'
#' @param a Vector or value to test
#' @param b Vector to test against
#'
#' @return Logical vector with TRUE for elements of `a` that are not in `b`
`%notin%` <- function(a, b) {
  !(a %in% b)
}

# not all in
#' Not All In Operator
#'
#' Tests if not all elements of `a` are contained in `b`.
#'
#' @param a Vector to test
#' @param b Vector to test against
#'
#' @return TRUE if at least one element of `a` is not in `b`, otherwise FALSE
`%notallin%` <- function(a, b) {
  !(all(a %in% b))
}
