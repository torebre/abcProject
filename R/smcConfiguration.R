#' Delegates to the implementation of print
#' in the object given as input.
#'
#' @export
print.smcConfiguration <- function(x, ...) {
  x$print(...)
}
