#' Convert layr object to ggplot2 object
#'
#' @param layr_obj A layr object
#' @return A ggplot object
#' @export
#' @examples
#' \dontrun{
#' viz <- visualize(mtcars, ~mpg ~ wt) |>
#'     add_guide(guide_scatter())
#' gg_plot <- as_ggplot(viz)
#' }
as_ggplot <- function(layr_obj) {
    if (!inherits(layr_obj, "layr")) {
        stop("Input must be a layr object")
    }

    # Start with basic ggplot
    gg <- ggplot2::ggplot(layr_obj$data)

    # Add basic mapping
    if (!is.null(layr_obj$mapping$x_var)) {
        gg <- gg + ggplot2::aes_string(x = layr_obj$mapping$x_var)
    }
    if (!is.null(layr_obj$mapping$y_var)) {
        gg <- gg + ggplot2::aes_string(y = layr_obj$mapping$y_var)
    }

    # Add guides
    for (guide in layr_obj$guides) {
        gg <- add_guide_to_ggplot(gg, guide)
    }

    # Add theme
    gg <- gg + ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    # Add title with narrative preview
    narrative_preview <- narrative(layr_obj, preview = TRUE)
    gg <- gg + ggplot2::ggtitle(narrative_preview)

    return(gg)
}

#' Add guide to ggplot
#' @keywords internal
add_guide_to_ggplot <- function(gg_obj, guide) {
    mapping_args <- list()

    # Convert layr mapping to ggplot mapping
    if (!is.null(guide$mapping)) {
        for (aes_name in names(guide$mapping)) {
            mapping_args[[aes_name]] <- guide$mapping[[aes_name]]
        }
    }

    # Create aesthetic mapping
    aes_mapping <- do.call(ggplot2::aes_string, mapping_args)

    # Get the appropriate geom function
    geom_func <- switch(guide$geom_type,
        "point" = ggplot2::geom_point,
        "line" = ggplot2::geom_line,
        "col" = ggplot2::geom_col,
        "density" = ggplot2::geom_density,
        ggplot2::geom_point # default
    )

    # Add parameters
    params <- guide$params

    return(gg_obj + do.call(geom_func, c(list(mapping = aes_mapping), params)))
}

#' View through lenses (faceting)
#'
#' @param layr_obj A layr object
#' @param formula Faceting formula
#' @return Modified layr object
#' @export
view_through <- function(layr_obj, formula) {
    if (!inherits(layr_obj, "layr")) {
        stop("First argument must be a layr object")
    }

    if (!rlang::is_formula(formula)) {
        stop("formula must be a formula object")
    }

    layr_obj$lenses <- c(layr_obj$lenses, list(formula))

    # Add to narrative
    form_chr <- as.character(formula)
    lens_narrative <- paste("Viewing through the lens of", form_chr[2])
    layr_obj$narrative_elements$lenses <- c(
        layr_obj$narrative_elements$lenses,
        lens_narrative
    )

    return(layr_obj)
}
