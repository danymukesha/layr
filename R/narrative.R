#' Generate narrative description of visualization
#'
#' @param layr_obj A layr object
#' @param preview Logical, if TRUE returns short preview
#' @return Character string with narrative
#' @export
#' @examples
#' \dontrun{
#' viz <- visualize(mtcars, ~mpg ~ wt)
#' narrative(viz)
#' }
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
        valid_annotations <- annotation_text[annotation_text != ""]
        if (length(valid_annotations) > 0) {
            narrative_parts$annotations <- paste(
                "Key observations:",
                paste(valid_annotations, collapse = "; ")
            )
        }
    }

    # Cognitive checks
    if (length(layr_obj$cognitive_checks) > 0) {
        narrative_parts$checks <- paste(
            "Design considerations:",
            paste(unique(layr_obj$cognitive_checks), collapse = "; ")
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
    if (length(narrative_parts) > 0) {
        final_narrative <- paste(
            unlist(narrative_parts),
            collapse = "\n\n"
        )
    } else {
        final_narrative <- "No narrative elements to display."
    }

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
        x_var_name <- mapping$x_var

        if (is.numeric(x_var)) {
            x_stats <- summary(x_var)
            insights <- c(
                insights,
                paste(
                    "The variable", x_var_name, "ranges from",
                    round(x_stats["Min."], 2), "to", round(x_stats["Max."], 2),
                    "with median", round(x_stats["Median"], 2)
                )
            )

            # Check for skewness
            if (!any(is.na(x_var))) {
                x_skew <- moments::skewness(x_var)
                if (abs(x_skew) > 1) {
                    direction <- ifelse(x_skew > 0, "right", "left")
                    insights <- c(
                        insights,
                        paste(x_var_name, "shows", direction, "skewness.")
                    )
                }
            }
        } else if (is.factor(x_var) || is.character(x_var)) {
            n_categories <- length(unique(x_var))
            top_category <- names(sort(table(x_var), decreasing = TRUE))[1]
            insights <- c(
                insights,
                paste(
                    x_var_name, "has", n_categories, "categories.",
                    "Most frequent:", top_category
                )
            )
        }
    }

    if (!is.null(mapping$y_var) && mapping$y_var %in% names(data)) {
        y_var <- data[[mapping$y_var]]
        y_var_name <- mapping$y_var

        if (is.numeric(y_var)) {
            y_stats <- summary(y_var)
            insights <- c(
                insights,
                paste(
                    y_var_name, "has median", round(y_stats["Median"], 2),
                    "and shows",
                    ifelse(sd(y_var, na.rm = TRUE) > mean(y_var, na.rm = TRUE) * 0.5,
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
            complete_cases <- complete.cases(x_var, y_var)
            if (sum(complete_cases) > 2) {
                correlation <- cor(x_var[complete_cases], y_var[complete_cases])
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
    }

    if (length(insights) > 0) {
        return(paste(insights, collapse = " "))
    }

    return(NULL)
}
