#' Add annotation to layr visualization
#'
#' @param layr_obj A layr object
#' @param annotation An annotation object
#' @return Modified layr object
#' @export
add_annotation <- function(layr_obj, annotation) {
    if (!inherits(layr_obj, "layr")) {
        stop("First argument must be a layr object")
    }

    layr_obj$annotations <- c(layr_obj$annotations, list(annotation))
    return(layr_obj)
}

#' Create a trend line annotation
#'
#' @param method Statistical method for trend
#' @param story Narrative description
#' @param ... Additional parameters
#' @return An annotation object
#' @export
annotation_trend_line <- function(method = "lm", story = NULL, ...) {
    structure(
        list(
            type = "trend_line",
            method = method,
            story = story,
            params = list(...)
        ),
        class = "layr_annotation"
    )
}

#' Create a highlight annotation
#'
#' @param data Data to highlight
#' @param label Label for highlight
#' @param story Narrative description
#' @param ... Additional parameters
#' @return An annotation object
#' @export
annotation_highlight <- function(data, label = NULL, story = NULL, ...) {
    structure(
        list(
            type = "highlight",
            data = data,
            label = label,
            story = story,
            params = list(...)
        ),
        class = "layr_annotation"
    )
}
