#' Convert layr visualization to interactive plot
#'
#' @param layr_obj A layr object
#' @return An interactive plotly object
#' @export
#' @examples
#' \dontrun{
#' if (require(plotly)) {
#'     viz <- visualize(mtcars, ~mpg ~ wt) |>
#'         add_guide(guide_scatter())
#'     make_playable(viz)
#' }
#' }
make_playable <- function(layr_obj) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("plotly package required for interactive plots. Install with install.packages('plotly')")
    }

    # First create ggplot2 object
    gg_obj <- as_ggplot(layr_obj)

    # Convert to plotly
    interactive_plot <- plotly::ggplotly(gg_obj)

    # Add narrative as annotation or tooltip
    narrative_text <- narrative(layr_obj, preview = TRUE)
    interactive_plot <- interactive_plot |>
        plotly::layout(
            title = list(
                text = "Interactive Visualization",
                x = 0,
                xanchor = "left"
            ),
            margin = list(t = 60),
            annotations = list(
                x = 0,
                y = 1.05,
                xref = "paper",
                yref = "paper",
                text = narrative_text,
                showarrow = FALSE,
                xanchor = "left",
                align = "left",
                font = list(size = 12)
            )
        )

    return(interactive_plot)
}
