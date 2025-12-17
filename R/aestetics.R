#' Create aesthetic mapping for layr
#'
#' @param ... Aesthetic mappings
#' @return A layr aesthetic mapping object
#' @export
#' @examples
#' \dontrun{
#' aes_semiotic(color = "species", size = "petal_length")
#' }
aes_semiotic <- function(...) {
    mapping <- list(...)

    # Validate that all mappings are character strings
    for (i in seq_along(mapping)) {
        if (!is.character(mapping[[i]]) || length(mapping[[i]]) != 1) {
            stop("All aesthetic mappings must be single character strings")
        }
    }

    class(mapping) <- "layr_aes"
    return(mapping)
}

#' Automatic aesthetic mapping with intelligent defaults
#'
#' @param data The data frame
#' @param x X variable
#' @param y Y variable
#' @param ... Additional mappings
#' @return A layr aesthetic mapping object
#' @export
aes_auto <- function(data, x, y = NULL, ...) {
    if (!is.data.frame(data)) {
        stop("data must be a data frame")
    }

    mapping <- list(...)

    if (!missing(x)) {
        if (!x %in% names(data)) {
            stop("x variable '", x, "' not found in data")
        }
        mapping$x <- x
    }

    if (!missing(y)) {
        if (!y %in% names(data)) {
            stop("y variable '", y, "' not found in data")
        }
        mapping$y <- y
    }

    # Intelligent defaults based on data types
    for (var_name in names(mapping)) {
        var <- mapping[[var_name]]
        if (is.character(var) && length(var) == 1) {
            if (var %in% names(data)) {
                var_data <- data[[var]]

                # Suggest better encodings based on data type
                if (var_name == "color" && is.numeric(var_data)) {
                    message("Consider using fill_band for continuous variable '", var, "'")
                }

                if (var_name == "shape" && length(unique(var_data)) > 6) {
                    message(
                        "Many categories (", length(unique(var_data)),
                        ") for shape encoding. Consider using color instead."
                    )
                }
            }
        }
    }

    class(mapping) <- "layr_aes"
    return(mapping)
}
