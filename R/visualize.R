#' @import ggplot2
#' @import dplyr
NULL

#' Create a new layr visualization
#'
#' @param data A data frame
#' @param formula A formula defining the data story (y ~ x)
#' @param ... Additional arguments
#' @return A layr object
#' @export
visualize <- function(data, formula = NULL, ...) {
    # Parse the formula
    vars <- parse_formula(formula)

    # Create the layr object
    layr_obj <- structure(
        list(
            data = data,
            mapping = vars,
            guides = list(),
            annotations = list(),
            lenses = list(),
            narrative_elements = list(),
            settings = list(...)
        ),
        class = "layr"
    )

    # Store narrative context
    layr_obj$narrative_elements$core_story <- create_core_story_narrative(vars, data)

    return(layr_obj)
}

#' Parse formula for layr
#' @keywords internal
parse_formula <- function(formula) {
    if (is.null(formula)) {
        return(list())
    }

    # Convert formula to character and parse
    form_chr <- as.character(formula)

    if (length(form_chr) == 3) {
        # We have y ~ x
        return(list(
            y_var = form_chr[2],
            x_var = form_chr[3]
        ))
    } else if (length(form_chr) == 2) {
        # We have ~ x (just one variable)
        return(list(
            x_var = form_chr[2]
        ))
    } else {
        stop("Invalid formula format. Use y ~ x or ~ x")
    }
}

#' Create core story narrative
#' @keywords internal
create_core_story_narrative <- function(vars, data) {
    if (length(vars) == 0) {
        return("Exploring the dataset structure.")
    }

    x_var <- vars$x_var
    y_var <- vars$y_var

    if (!is.null(y_var) && !is.null(x_var)) {
        x_type <- infer_variable_type(data[[x_var]])
        y_type <- infer_variable_type(data[[y_var]])

        return(paste0(
            "This visualization explores the relationship between ",
            y_var, " (", y_type, ") and ",
            x_var, " (", x_type, ")."
        ))
    } else if (!is.null(x_var)) {
        x_type <- infer_variable_type(data[[x_var]])
        return(paste0(
            "This visualization shows the distribution of ",
            x_var, " (", x_type, ")."
        ))
    }
}

#' Infer variable type
#' @keywords internal
infer_variable_type <- function(x) {
    if (is.numeric(x)) {
        "continuous"
    } else if (is.factor(x) || is.character(x)) {
        "categorical"
    } else if (is.logical(x)) {
        "binary"
    } else if (lubridate::is.Date(x) || lubridate::is.POSIXt(x)) {
        "temporal"
    } else {
        "unknown"
    }
}

#' Print method for layr objects
#' @export
print.layr <- function(x, ...) {
    cat("layr Visualization\n")
    cat("==================\n")
    cat("Data:", nrow(x$data), "rows ×", ncol(x$data), "columns\n")

    if (!is.null(x$mapping$x_var)) {
        if (!is.null(x$mapping$y_var)) {
            cat("Story:", x$mapping$y_var, "~", x$mapping$x_var, "\n")
        } else {
            cat("Story: ~", x$mapping$x_var, "\n")
        }
    }

    cat("Guides:", length(x$guides), "\n")
    cat("Annotations:", length(x$annotations), "\n")
    cat("Lenses:", length(x$lenses), "\n")

    # Show narrative preview
    narrative_preview <- narrative(x, preview = TRUE)
    cat("\nNarrative Preview:\n")
    cat(strwrap(narrative_preview, width = 60), sep = "\n")

    invisible(x)
}
