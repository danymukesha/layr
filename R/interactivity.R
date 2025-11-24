#' Convert layr visualization to interactive plot
#'
#' @param layr_obj A layr object
#' @return An interactive plotly object
#' @export
make_playable <- function(layr_obj) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("plotly package required for interactive plots")
    }

    # First create ggplot2 object
    gg_obj <- as_ggplot(layr_obj)

    # Convert to plotly
    interactive_plot <- plotly::ggplotly(gg_obj)

    # Add narrative as annotation or tooltip
    narrative_text <- narrative(layr_obj, preview = TRUE)
    interactive_plot <- interactive_plot |>
        plotly::layout(
            title = paste("Interactive Visualization"),
            annotations = list(
                text = narrative_text,
                xref = "paper", yref = "paper",
                x = 0, y = 1.1,
                showarrow = FALSE,
                font = list(size = 10)
            )
        )

    return(interactive_plot)
}
