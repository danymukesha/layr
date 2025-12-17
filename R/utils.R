#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom purrr %>%
#' @usage lhs \%>\% rhs
NULL

#' Check if object is a layr object
#'
#' @param x Object to check
#' @return Logical
#' @export
is.layr <- function(x) {
    inherits(x, "layr")
}

#' Check if object is a layr guide
#'
#' @param x Object to check
#' @return Logical
#' @export
is.layr_guide <- function(x) {
    inherits(x, "layr_guide")
}

#' Check if object is a layr annotation
#'
#' @param x Object to check
#' @return Logical
#' @export
is.layr_annotation <- function(x) {
    inherits(x, "layr_annotation")
}
