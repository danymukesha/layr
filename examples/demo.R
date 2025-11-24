# Load the package (after installing it)
# devtools::load_all(".")

library(dplyr)
library(gapminder)

# Example 1: Basic scatter plot with narrative
plot1 <- visualize(gapminder, ~lifeExp ~ gdpPercap) |>
    add_guide(guide_scatter(aes_semiotic(color = continent), alpha = 0.6))

print(plot1)
cat("\nFull Narrative:\n")
cat(narrative(plot1))

# Example 2: More complex visualization
plot2 <- visualize(gapminder, ~lifeExp ~ gdpPercap) |>
    add_guide(guide_scatter(
        aes_semiotic(color = continent, size = pop),
        alpha = 0.6
    )) |>
    add_annotation(
        annotation_trend_line(
            method = "loess",
            story = "Overall positive relationship with diminishing returns"
        )
    ) |>
    add_annotation(
        annotation_highlight(
            data = gapminder |> filter(country == "China"),
            label = "China's development",
            story = "Rapid economic growth with improving life expectancy"
        )
    )

print(plot2)

# Generate the complete narrative
cat("\n", narrative(plot2))

# Create interactive version
if (require(plotly)) {
    interactive_plot <- make_playable(plot2)
    print(interactive_plot)
}

# Example 3: Line plot for time series
plot3 <- visualize(gapminder, ~lifeExp ~ year) |>
    add_guide(guide_line(aes_semiotic(group = country, color = continent), alpha = 0.3)) |>
    add_annotation(
        annotation_highlight(
            data = gapminder |> filter(country == "Rwanda"),
            story = "Dramatic recovery post-1994 genocide"
        )
    )

print(plot3)
cat("\n", narrative(plot3))
