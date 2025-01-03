# not in
`%notin%` <- function(a, b) {
  !(a %in% b)
}

# not all in
`%notallin%` <- function(a, b) {
  !(all(a %in% b))
}

# infix for null replacement
#' @importFrom rlang `%||%`
`%ifnullrep%` <- function(a, b) {
  a %||% b
}
