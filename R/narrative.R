#' Generate narrative description of visualization
#'
#' @param layr_obj A layr object
#' @param preview Logical, if TRUE returns short preview
#' @return Character string with narrative
#' @export
narrative <- function(layr_obj, preview = FALSE) {
    if (!inherits(layr_obj, "layr")) {
        stop("Input must be a layr object")
    }

    narrative_parts <- list()

    # Core story
    if (!is.null(layr_obj$narrative_elements$core_story)) {
        narrative_parts$core <- layr_obj$narrative_elements$core_story
    }

    # Guides description
    if (length(layr_obj$narrative_elements$guides) > 0) {
        narrative_parts$guides <- paste(
            layr_obj$narrative_elements$guides,
            collapse = " "
        )
    }

    # Data insights
    data_insights <- generate_data_insights(layr_obj)
    if (!is.null(data_insights)) {
        narrative_parts$insights <- data_insights
    }

    # Annotations
    if (length(layr_obj$annotations) > 0) {
        annotation_text <- sapply(layr_obj$annotations, function(ann) {
            if (!is.null(ann$story)) ann$story else ""
        })
        narrative_parts$annotations <- paste(
            "Key observations:",
            paste(annotation_text[annotation_text != ""], collapse = "; ")
        )
    }

    if (preview) {
        # Return just the core story for preview
        if (!is.null(narrative_parts$core)) {
            return(narrative_parts$core)
        } else {
            return("No narrative available.")
        }
    }

    # Combine all narrative parts
    final_narrative <- paste(
        unlist(narrative_parts),
        collapse = "\n\n"
    )

    return(final_narrative)
}

#' Generate data insights for narrative
#' @keywords internal
generate_data_insights <- function(layr_obj) {
    insights <- character()
    data <- layr_obj$data
    mapping <- layr_obj$mapping

    if (!is.null(mapping$x_var) && mapping$x_var %in% names(data)) {
        x_var <- data[[mapping$x_var]]

        if (is.numeric(x_var)) {
            x_stats <- summary(x_var)
            insights <- c(
                insights,
                paste(
                    "The variable", mapping$x_var, "ranges from",
                    round(x_stats["Min."], 2), "to", round(x_stats["Max."], 2),
                    "with median", round(x_stats["Median"], 2)
                )
            )
        }
    }

    if (!is.null(mapping$y_var) && mapping$y_var %in% names(data)) {
        y_var <- data[[mapping$y_var]]

        if (is.numeric(y_var)) {
            y_stats <- summary(y_var)
            insights <- c(
                insights,
                paste(
                    mapping$y_var, "has median", round(y_stats["Median"], 2),
                    "and shows", ifelse(sd(y_var, na.rm = TRUE) > mean(y_var, na.rm = TRUE) * 0.5,
                        "high", "moderate"
                    ), "variability."
                )
            )
        }
    }

    # Relationship insights for scatter plots
    if (!is.null(mapping$x_var) && !is.null(mapping$y_var) &&
        mapping$x_var %in% names(data) && mapping$y_var %in% names(data)) {
        x_var <- data[[mapping$x_var]]
        y_var <- data[[mapping$y_var]]

        if (is.numeric(x_var) && is.numeric(y_var)) {
            correlation <- cor(x_var, y_var, use = "complete.obs")
            if (!is.na(correlation)) {
                strength <- ifelse(abs(correlation) > 0.7, "strong",
                    ifelse(abs(correlation) > 0.3, "moderate", "weak")
                )
                direction <- ifelse(correlation > 0, "positive", "negative")

                insights <- c(
                    insights,
                    paste(
                        "There is a", strength, direction, "relationship between the variables",
                        "(r =", round(correlation, 2), ")."
                    )
                )
            }
        }
    }

    if (length(insights) > 0) {
        return(paste(insights, collapse = " "))
    }

    return(NULL)
}
