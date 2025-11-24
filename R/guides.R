#' Add a guide to a layr visualization
#'
#' @param layr_obj A layr object
#' @param guide A guide object
#' @return Modified layr object
#' @export
add_guide <- function(layr_obj, guide) {
    if (!inherits(layr_obj, "layr")) {
        stop("First argument must be a layr object")
    }

    layr_obj$guides <- c(layr_obj$guides, list(guide))

    # Add to narrative elements
    guide_narrative <- create_guide_narrative(guide, layr_obj$mapping)
    layr_obj$narrative_elements$guides <- c(
        layr_obj$narrative_elements$guides,
        guide_narrative
    )

    # Perform cognitive checks
    cognitive_check <- perform_cognitive_check(layr_obj, guide)
    if (!is.null(cognitive_check)) {
        message("Cognitive Check: ", cognitive_check)
    }

    return(layr_obj)
}

#' Create a scatter plot guide
#'
#' @param mapping Aesthetic mapping
#' @param ... Additional parameters
#' @return A guide object
#' @export
guide_scatter <- function(mapping = NULL, ...) {
    structure(
        list(
            type = "scatter",
            mapping = mapping,
            params = list(...),
            geom_type = "point"
        ),
        class = "layr_guide"
    )
}

#' Create a line guide
#'
#' @param mapping Aesthetic mapping
#' @param ... Additional parameters
#' @return A guide object
#' @export
guide_line <- function(mapping = NULL, ...) {
    structure(
        list(
            type = "line",
            mapping = mapping,
            params = list(...),
            geom_type = "line"
        ),
        class = "layr_guide"
    )
}

#' Create a bar guide
#'
#' @param mapping Aesthetic mapping
#' @param ... Additional parameters
#' @return A guide object
#' @export
guide_bar <- function(mapping = NULL, ...) {
    structure(
        list(
            type = "bar",
            mapping = mapping,
            params = list(...),
            geom_type = "col"
        ),
        class = "layr_guide"
    )
}

#' Create narrative for a guide
#' @keywords internal
create_guide_narrative <- function(guide, mapping) {
    base_desc <- switch(guide$type,
        "scatter" = "Scatter points show individual observations.",
        "line" = "Lines connect sequential observations.",
        "bar" = "Bars represent aggregated values.",
        "A visual representation of the data."
    )

    # Add mapping information to narrative
    if (!is.null(guide$mapping)) {
        mapping_desc <- describe_mapping_narrative(guide$mapping)
        return(paste(base_desc, mapping_desc))
    }

    return(base_desc)
}

#' Describe mapping for narrative
#' @keywords internal
describe_mapping_narrative <- function(mapping) {
    desc <- character()

    if (!is.null(mapping$color)) {
        desc <- c(desc, paste("Color represents", mapping$color))
    }
    if (!is.null(mapping$size)) {
        desc <- c(desc, paste("Size represents", mapping$size))
    }
    if (!is.null(mapping$group)) {
        desc <- c(desc, paste("Grouped by", mapping$group))
    }

    if (length(desc) > 0) {
        return(paste("Additional encodings:", paste(desc, collapse = "; ")))
    }

    return("")
}

#' Perform cognitive load checks
#' @keywords internal
perform_cognitive_check <- function(layr_obj, guide) {
    checks <- list()

    # Check for overplotting in scatter plots
    if (guide$type == "scatter" && nrow(layr_obj$data) > 1000) {
        checks <- c(
            checks,
            "High number of points may cause overplotting. Consider using transparency or sampling."
        )
    }

    # Check for too many categories in color mapping
    if (!is.null(guide$mapping$color)) {
        color_var <- guide$mapping$color
        if (color_var %in% names(layr_obj$data)) {
            n_categories <- length(unique(layr_obj$data[[color_var]]))
            if (n_categories > 10) {
                checks <- c(
                    checks,
                    paste("Many categories (", n_categories, ") for color encoding. Consider simplifying.")
                )
            }
        }
    }

    if (length(checks) > 0) {
        return(paste(checks, collapse = " "))
    }

    return(NULL)
}
