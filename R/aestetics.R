#' Create aesthetic mapping for layr
#'
#' @param ... Aesthetic mappings
#' @return A layr aesthetic mapping object
#' @export
aes_semiotic <- function(...) {
    mapping <- list(...)
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
    mapping <- list(...)

    if (!missing(x)) mapping$x <- x
    if (!missing(y)) mapping$y <- y

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
                    message("Many categories for shape encoding. Consider using color instead.")
                }
            }
        }
    }

    class(mapping) <- "layr_aes"
    return(mapping)
}
